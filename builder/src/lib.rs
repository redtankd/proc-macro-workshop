extern crate proc_macro;

use std::result::Result;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::spanned::Spanned;
use syn::*;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let ItemStruct {
        vis, ident, fields, ..
    } = parse_macro_input!(input);

    let ident_for_span = ident.clone();

    let builder_ident = format!("{}Builder", ident);
    let builder_ident = Ident::new(&builder_ident, Span::call_site());

    match fields {
        Fields::Named(fields) => {
            match make_fields(fields) {
                Ok((builder_fields, builder_setter, builder_build,
                    builder_build_value, builder_init)) => {
                    (quote! {
                        #vis struct #builder_ident {
                            #(#builder_fields)*
                        }

                        impl #builder_ident {
                            #(#builder_setter)*

                            fn build(&mut self) -> std::result::Result<#ident, std::boxed::Box<dyn std::error::Error>> {
                                #(#builder_build)*

                                return std::result::Result::Ok(#ident {
                                    #(#builder_build_value)*
                                });
                            }
                        }

                        impl #ident {
                            pub fn builder() -> #builder_ident {
                                #builder_ident {
                                    #(#builder_init)*
                                }
                            }
                        }
                    })
                    .into()
                }
                Err(token_stream) =>{
                    token_stream
                }
            }
        }
        _ => {
            return syn::Error::new_spanned(ident_for_span, "named fields expected")
                .to_compile_error()
                .into();
        }
    }
}

fn make_fields(
    fields: FieldsNamed,
) -> Result<
    (
        Vec<proc_macro2::TokenStream>,
        Vec<proc_macro2::TokenStream>,
        Vec<proc_macro2::TokenStream>,
        Vec<proc_macro2::TokenStream>,
        Vec<proc_macro2::TokenStream>,
    ),
    TokenStream,
> {
    let mut builder_fields = Vec::<proc_macro2::TokenStream>::new();
    let mut builder_setter = Vec::<proc_macro2::TokenStream>::new();
    let mut builder_build = Vec::<proc_macro2::TokenStream>::new();
    let mut builder_build_value = Vec::<proc_macro2::TokenStream>::new();
    let mut builder_init = Vec::<proc_macro2::TokenStream>::new();

    for field in fields.named {
        // the field's name
        let i = field.ident.clone().unwrap();

        // the field's type
        let ty = field.ty.clone();

        // check if the field's type is Option or Vec
        let (ty_name, ty_generics) = parse_generic_type(&ty)?;
        let ty_is_option = if "Option" == ty_name.to_string() {
            true
        } else {
            false
        };
        let ty_is_vec = if "Vec" == ty_name.to_string() {
            true
        } else {
            false
        };
        let ty_generic_arg = if ty_is_option || ty_is_vec {
            ty_generics[0].clone()
        } else {
            ty.clone()
        };

        // repeated setter for Vec field
        let repeated_setter_name = parse_attr_builder(&field)?;

        let error_msg = format!("the field {} is None", i);

        if repeated_setter_name.is_some() && ty_is_vec {
            let setter_name = Ident::new(&repeated_setter_name.unwrap(), Span::call_site());

            // construct the builder's field, which is the Vec type
            builder_fields.push(quote! {
                #i: #ty,
            });

            // construct the builder's repeated setter function
            // the function name is from attribute.
            builder_setter.push(quote! {
                fn #setter_name(&mut self, #setter_name: #ty_generic_arg) -> &mut Self {
                    self.#i.push(#setter_name);
                    self
                }
            });

            builder_build.push(quote! {
                let #i = self.#i.clone();
            });

            // builder's initial value
            builder_init.push(quote! {
                #i: std::vec::Vec::new(),
            });
        } else if ty_is_option {
            // construct the builder's field, which is the Option type
            builder_fields.push(quote! {
                #i: #ty,
            });

            // construct the builder's setter functions
            builder_setter.push(quote! {
                fn #i(&mut self, #i: #ty_generic_arg) -> &mut Self {
                    self.#i = std::option::Option::Some(#i);
                    self
                }
            });

            builder_build.push(quote! {
                let #i = self.#i.clone();
            });

            // builder's initial value
            builder_init.push(quote! {
                #i: std::option::Option::None,
            });
        } else {
            // construct the builder's field, which wrap the original type
            builder_fields.push(quote! {
                #i: std::option::Option<#ty>,
            });

            builder_setter.push(quote! {
                fn #i(&mut self, #i: #ty) -> &mut Self {
                    self.#i = std::option::Option::Some(#i);
                    self
                }
            });
            // check if the value is exist for non-option field
            builder_build.push(quote! {
                let #i = match self.#i.clone() {
                    std::option::Option::Some(v) => v,
                    _ => {
                        return std::result::Result::Err(std::convert::From::from(#error_msg));
                    }
                };
            });

            // builder's initial value
            builder_init.push(quote! {
                #i: std::option::Option::None,
            });
        }

        // the builder return value's fields
        builder_build_value.push(quote! {
            #i: #i,
        });
    }

    Ok((
        builder_fields,
        builder_setter,
        builder_build,
        builder_build_value,
        builder_init,
    ))
}

// the syntax tree for types
//
//     Type::Path(
//         TypePath {
//             qself: None,
//             path: Path {
//                 segments: [
//                     PathSegment {
//                         ident: "Option",
//                         arguments: PathArguments::AngleBracketed(
//                             AngleBracketedGenericArguments {
//                                 args: [
//                                     GenericArgument::Type(
//                                         ...
//                                     ),
//                                 ],
//                             },
//                         ),
//                     },
//                 ],
//             },
//         },
//     )
fn parse_generic_type(ty: &Type) -> Result<(Ident, Vec<Type>), TokenStream> {
    let mut generic_tys = Vec::<Type>::new();
    match ty {
        Type::Path(ty) => match ty.path.segments.iter().last() {
            Some(PathSegment { ident, arguments }) => match arguments {
                PathArguments::AngleBracketed(arguments) => {
                    for arg in arguments.args.iter() {
                        match arg {
                            GenericArgument::Type(generic_ty) => {
                                generic_tys.push(generic_ty.clone())
                            }
                            _ => {}
                        }
                    }
                    return Ok((ident.clone(), generic_tys));
                }
                PathArguments::None => return Ok((ident.clone(), generic_tys)),
                _ => {}
            },
            _ => {}
        },
        _ => {}
    }

    Err(syn::Error::new(ty.span(), "can't parse the type")
        .to_compile_error()
        .into())
}

// repeated field for #[builder(each = "xxx")]
// return attr's value "xxx"
fn parse_attr_builder(field: &Field) -> Result<Option<String>, TokenStream> {
    for attr in field.attrs.clone() {
        match attr.parse_meta() {
            Ok(Meta::List(attr)) => {
                if "builder" == attr.ident.to_string() {
                    if attr.nested.len() == 1 {
                        match attr.nested.iter().next() {
                            Some(NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                                ident: ident_value,
                                lit: Lit::Str(lit_str),
                                ..
                            }))) => {
                                if "each" == ident_value.to_string() {
                                    return Ok(Some(lit_str.value()));
                                }
                            }
                            _ => {}
                        }
                    }
                    return Err(syn::Error::new_spanned(
                        attr,
                        "expected `builder(each = \"...\")`",
                    )
                    .to_compile_error()
                    .into());
                }
            }
            Ok(_) => {}
            Err(_) => {}
        }
    }

    return Ok(None);
}
