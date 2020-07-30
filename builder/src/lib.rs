extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{Expr, Type, GenericArgument, parse_macro_input, DeriveInput};
use syn::PathArguments::AngleBracketed;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    dbg!(&ast);
    let name = &ast.ident;
    let builder = syn::Ident::new(&format!("{}Builder", name), name.span());
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { named, .. }),
        ..
    }) = &ast.data
    {
        named
    } else {
        unimplemented!();
    };
    let bconstructor = fields.iter().map(|f| {
        dbg!(attribute_each(f));
        let ident = &f.ident;
        if is_optional(f) {
            quote! {
                #ident: self.#ident.clone()
            }
        } else {
            quote! {
                #ident: self.#ident.clone().ok_or(concat!(stringify!(#ident), " is required"))?
            }
        }
    });
    let bdefaults = fields.iter().map(|f| {
        let ident = &f.ident;
        quote! {
            #ident: None
        }
    });
    let bfields = fields.iter().map(|f| {
        let ty = &f.ty;
        let ident = &f.ident;
        if is_optional(f) {
            quote! {
                #ident: #ty
            }
        } else {
            quote! {
                #ident: Option<#ty>
            }
        }
    });
    let bmethods = fields.iter().map(|f| {
        let ty = unnest_option(&f.ty).unwrap_or(&f.ty);
        let ident = &f.ident;
        quote! {
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        }
    });
    let expanded = quote! {
        pub struct #builder {
            #(#bfields),*
        }

        impl #builder {
            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #(#bconstructor),*
                })
            }

            #(#bmethods)*
        }

        impl #name {
            pub fn builder() -> #builder {
                #builder {
                    #(#bdefaults),*
                }
            }
        }
    };
    TokenStream::from(expanded)
}

fn is_optional(field: &syn::Field) -> bool {
    if let syn::Type::Path(ref p) = field.ty {
        p.path
            .segments
            .last()
            .map(|p| &p.value().ident)
            .map(|i| i == "Option")
            .unwrap_or_default()
    } else {
        false
    }
}

fn unnest_option(ty: &syn::Type) -> Option<&syn::Type> {
    if let Type::Path(expr) = ty {
        let ident = expr.path.segments
            .last()
            .filter(|p| &p.value().ident == "Option")
            .map(|p| &p.value().arguments);
        if let Some(AngleBracketed(expr)) = ident {
            if let Some(last) = expr.args.last() {
                if let GenericArgument::Type(expr) = last.value() {
                    return Some(expr);
                }
            }
        }
    }
    None
}

fn attribute_each(field: &syn::Field) -> Option<String> {
    let meta: Vec<syn::Meta> = field.attrs.iter()
        .filter(|a| a.path.segments.last().map(|p| p.value().ident == "builder").unwrap_or_default())
        .map(|a| a.parse_meta().unwrap()).collect();
    if let Some(expr) = meta.first() {
        if let syn::Meta::List(expr) = expr {
            if let Some(expr) =  expr.nested.last() {
                if let syn::NestedMeta::Meta(expr) =  expr.value() {
                    if let syn::Meta::NameValue(expr) = expr {
                        if let syn::Lit::Str(expr) = &expr.lit {
                            return Some(expr.value());
                        }
                    }
                }
            }
        }
    }
    None
}
