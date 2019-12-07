extern crate proc_macro;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, Attribute, Data, DataEnum, DeriveInput, Expr, Field, Fields, GenericParam,
    Ident, Lit, Meta, NestedMeta, Type, Variant,
};

/// Automatically derive the `FiniteSet` trait for structs and enums
///
/// This trait can be applied to any `struct` or `enum` where all fields themselves implement the
/// [`FiniteSet`](../finite_model/trait.FiniteSet.html) trait. The includes all integer numeric
/// values, `bool`, and `()`.
///
/// ```rust
/// #[derive(Clone, FiniteSet)]
/// enum State {
///     Initial,
///     First(i32, u64),
///     Second(bool),
///     Final {
///         count: u8,
///     }
/// }
/// ```
///
/// Constraining integer types
/// ==========================
///
/// By default integer types will iterate through every value that can be expressed in that type
/// from minimum to maximum. These values can be limited in order to constrain the state space and
/// reduce checking of similar states. Limiting simply requires adding a `values` attribute to
/// fields with numeric value types. The value associated with `values` must be expressions that
/// can be converted into an iterator over values of the type.
///
/// ```rust
/// #[derive(Clone, FiniteSet)]
/// enum State {
///     Initial,
///     First {
///         #[finite_set(values = "vec![8, 16, 32]")]
///         alpha: i32,
///         #[finite_set(values = "12..8")]
///         beta: u64,
///     },
///     Second(bool),
///     Final(#[finite_set(values = "(16..24).chain(32..=48).map(|x| x * 2)")] u8)
/// }
/// ```
///
/// Working with collections
/// ========================
///
/// Whilst [`FiniteSet`](../finite_model/trait.FiniteSet.html) cannot be derived for strings or
/// arrays, collections such as `Vec` and `HashSet` can be used. Collections can only be used when
/// explicit lengths are specified. In a similar manner to specifying a specific set of values to
/// use for certain types, a specific set of sizes can be provided for collections. Collections
/// must simply be provided a `lengths` attribute with an expression that can be converted into an
/// iterator over values of type `usize`.
///
/// ```rust
/// #[derive(Clone, FiniteSet)]
/// struct State {
///     #[finite_set(values = "0..12")]
///     alpha: u64,
///     #[finite_set(lengths = "vec![3, 5, 7]")]
///     beta: Vec<bool>,
/// }
/// ```
#[proc_macro_derive(FiniteSet, attributes(finite_set))]
pub fn derive_finite_set(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;
    let long_generics = Generics::Constrained.for_item(&input);
    let short_generics = Generics::Unconstrained.for_item(&input);

    let (iterator, combinator, impls) = match &input.data {
        Data::Struct(data) => (
            struct_iterator(&data.fields),
            struct_combinator(StructIdentBuilder, &data.fields),
            struct_impls(StructIdentBuilder, &data.fields),
        ),
        Data::Enum(data) => (
            enum_iterator(&data),
            enum_combinator(&data),
            enum_impls(&data),
        ),
        Data::Union(_data) => panic!("Cannot derive FiniteSet on union"),
    };

    let tokens = quote! {
        impl<#(#long_generics),*> ::finite_model::FiniteSet for #name <#(#short_generics)*> {
            type Iter = #iterator;

            fn finite_set() -> Self::Iter {
                #combinator
            }
        }

        impl<#(#long_generics),*> #name <#(#short_generics)*> {
            #(#impls)*
        }
    };

    tokens.into()
}

/// Parsers for generic attributes to an item
enum Generics {
    Constrained,
    Unconstrained,
}

impl Generics {
    fn for_item(&self, item: &DeriveInput) -> Vec<TokenStream> {
        use Generics::*;
        item.generics
            .params
            .iter()
            .map(move |generic| match (self, generic) {
                (Constrained, GenericParam::Type(type_param)) => {
                    let name = &type_param.ident;
                    let bounds = &type_param.bounds;
                    quote! { #name: ::finite_model::FiniteSet + #bounds }
                }
                (Unconstrained, GenericParam::Type(type_param)) => {
                    let name = &type_param.ident;
                    quote! { #name }
                }
                (_, param) => quote! { #param },
            })
            .collect()
    }
}

/// Generate the type signature for the iterator over an enum type
fn enum_iterator(item: &DataEnum) -> TokenStream {
    let mut iterator = quote! { ::finite_model::EmptyIter<Self> };

    for variant in item.variants.iter() {
        let field_iterator = struct_iterator(&variant.fields);
        iterator = quote! { ::std::iter::Chain<#iterator, #field_iterator> };
    }

    iterator
}

/// Generate the type signature for the iterator over an struct type
fn struct_iterator(fields: &Fields) -> TokenStream {
    let mut iterator = quote! { ::finite_model::UnitIter };
    let mut joined = quote! { () };

    for field in fields.iter() {
        let field_type = &field.ty;
        let multiplier_type = FieldAttribute::multiplier_type(&field, &joined);
        iterator = quote! {
            ::std::iter::FlatMap<#iterator, #multiplier_type, fn(#joined) -> #multiplier_type>
        };
        joined = quote! { (#joined, #field_type) };
    }

    let struct_map_type = struct_map_type(fields);
    iterator = quote! { ::std::iter::Map<#iterator, #struct_map_type> };

    iterator
}

/// Generate the cobinator implementation used to implement finite_set for enums
fn enum_combinator(item: &DataEnum) -> TokenStream {
    let mut combinator = quote! { ::finite_model::EmptyIter::default() };

    for variant in item.variants.iter() {
        let ident_builder = EnumIdentBuilder::for_variant(&variant);
        let field_combinator = struct_combinator(ident_builder, &variant.fields);
        combinator = quote! {
            #combinator.chain(#field_combinator)
        };
    }

    combinator
}

/// Generate the cobinator implementation used to implement finite_set for structs
fn struct_combinator(ident_builder: impl FieldIdentBuilder, fields: &Fields) -> TokenStream {
    let mut combinator = quote! {
        ::finite_model::UnitIter::default()
    };

    let mut rec_type = quote! { () };

    for (field, field_name) in name_fields(fields) {
        let field_type = &field.ty;
        let joiner_name = ident_builder.field_join(&field_name);
        let multiplier_type = FieldAttribute::multiplier_type(&field, &rec_type);

        combinator = quote! {
            #combinator.flat_map(Self::#joiner_name as fn (#rec_type) -> #multiplier_type)
        };
        rec_type = quote! { (#rec_type, #field_type) };
    }

    let map_name = ident_builder.struct_map();
    let struct_map_type = struct_map_type(fields);
    combinator = quote! { #combinator.map(Self::#map_name as #struct_map_type) };

    combinator
}

/// Generate the additional functions required for an enum
fn enum_impls(item: &DataEnum) -> Vec<TokenStream> {
    item.variants
        .iter()
        .flat_map(|variant| struct_impls(EnumIdentBuilder::for_variant(&variant), &variant.fields))
        .collect()
}

/// Generate the additional join functions needed for a struct
fn struct_impls(ident_builder: impl FieldIdentBuilder, fields: &Fields) -> Vec<TokenStream> {
    let mut joiners = vec![];
    let mut rec_type = quote! { () };

    for (field, field_name) in name_fields(fields) {
        let field_type = &field.ty;
        let joiner_name = ident_builder.field_join(&field_name);
        let tail = format_ident!("tail");
        let multiplier_type = FieldAttribute::multiplier_type(&field, &rec_type);
        let multiplier_new = FieldAttribute::multiplier_new(&field, &tail);

        let joiner = quote! {
            #[allow(non_snake_case)]
            fn #joiner_name (#tail: #rec_type)
            -> #multiplier_type {
                #multiplier_new
            }
        };
        rec_type = quote! { (#rec_type, #field_type) };

        joiners.push(joiner);
    }

    joiners.push(struct_map_impl(ident_builder, fields));

    joiners
}

/// Generate map function for a single varaint or bare structure
fn struct_map_impl(ident_builder: impl FieldIdentBuilder, fields: &Fields) -> TokenStream {
    let struct_name = ident_builder.struct_name();
    let types = field_types(&fields);
    let args = name_fields(fields)
        .map(|(_, name)| name)
        .collect::<Vec<_>>();
    let body = match fields {
        Fields::Unit => struct_name,
        Fields::Unnamed(_) => {
            quote! {
                #struct_name (#(#args),*)
            }
        }
        Fields::Named(_) => {
            quote! {
                #struct_name {#(#args),*}
            }
        }
    };
    let rec_args = recursive_join(args);
    let rec_types = recursive_join(types);
    let map_name = ident_builder.struct_map();
    quote! {
        #[allow(non_snake_case)]
        fn #map_name( #rec_args: #rec_types ) -> Self {
            #body
        }
    }
}

/// Attributes that may be associated with fields for derive
enum FieldAttribute {
    /// Collections of a given length
    Lengths(TokenStream),
    /// Expression representing a particular set of values
    Values(TokenStream),
}

impl FieldAttribute {
    /// Get the first valid attribute
    fn first<'a>(attrs: impl IntoIterator<Item = &'a Attribute> + 'a) -> Option<Self> {
        Self::parse(attrs).next()
    }

    /// Type signature for the multiplier of a field
    fn multiplier_type(field: &Field, before: &TokenStream) -> TokenStream {
        let field_type = &field.ty;
        match Self::first(&field.attrs) {
            Some(Self::Lengths(_)) => {
                quote! {
                    ::finite_model::CollectionMultiplier<
                        Box<dyn Iterator<Item = usize>>,
                        #before,
                        #field_type,
                    >
                }
            }
            Some(Self::Values(_)) => {
                quote! {
                    ::finite_model::ValuesMultiplier<
                        Box<dyn Iterator<Item = #field_type>>,
                        #before,
                        #field_type,
                    >
                }
            }
            None => {
                quote! { ::finite_model::Multiplier<#before, #field_type> }
            }
        }
    }

    /// Type signature for the multiplier of a field
    fn multiplier_new(field: &Field, tail: &Ident) -> TokenStream {
        match Self::first(&field.attrs) {
            Some(Self::Lengths(lengths)) => {
                quote! {
                    ::finite_model::CollectionMultiplier::new(
                        Box::new(IntoIterator::into_iter(#lengths)),
                        #tail,
                    )
                }
            }
            Some(Self::Values(values)) => {
                quote! {
                    ::finite_model::ValuesMultiplier::new(
                        Box::new(IntoIterator::into_iter(#values)),
                        #tail,
                    )
                }
            }
            None => {
                quote! { ::finite_model::Multiplier::new(#tail) }
            }
        }
    }

    /// Parse attributes on field
    fn parse<'a>(
        attrs: impl IntoIterator<Item = &'a Attribute> + 'a,
    ) -> impl Iterator<Item = Self> + 'a {
        attrs
            .into_iter()
            .filter_map(|attr| attr.parse_meta().ok())
            .filter_map(|meta| match meta {
                Meta::List(list) => Some(list),
                _ => None,
            })
            .flat_map(|meta| meta.nested.into_iter())
            .filter_map(|nested| match nested {
                NestedMeta::Meta(meta) => Some(meta),
                _ => None,
            })
            .map(|meta| Self::parse_meta(meta).expect("Valid attribute"))
    }

    /// Parse a single metadata attribute
    fn parse_meta(meta: Meta) -> Result<Self, String> {
        match meta {
            Meta::NameValue(meta) if meta.path.is_ident("lengths") => Self::expr_value(meta.lit)
                .map(|lengths| Self::Lengths(quote! { #lengths }))
                .map_err(|err| format!("Bad 'range' attribute: {}", err)),
            Meta::NameValue(meta) if meta.path.is_ident("values") => Self::expr_value(meta.lit)
                .map(|values| Self::Values(quote! { #values }))
                .map_err(|err| format!("Bad 'bound' attribute: {}", err)),
            meta => Err(format!("Invalid attribute: {}", quote! { #meta })),
        }
    }

    /// Parse a literal into a range expression
    fn expr_value(value: Lit) -> Result<Expr, String> {
        if let Lit::Str(value) = value {
            value
                .parse::<Expr>()
                .map_err(|err| format!("Not a range expression: {}", err))
        } else {
            Err(format!("Not a string literal: {}", quote! { #value }))
        }
    }
}

/// Assign unique names to all fields in a struct
fn name_fields<'f>(
    fields: impl IntoIterator<Item = &'f Field>,
) -> impl Iterator<Item = (&'f Field, Ident)> {
    fields.into_iter().enumerate().map(|(i, f)| {
        (
            f,
            f.ident
                .as_ref()
                .cloned()
                .unwrap_or_else(|| format_ident!("_{}", i)),
        )
    })
}

/// Generate names for builders of iterators over values on given fields
trait FieldIdentBuilder {
    fn field_join(&self, field: &Ident) -> Ident;

    fn struct_name(&self) -> TokenStream;

    fn struct_map(&self) -> Ident;
}

/// Field identifier generator for bare structs
struct StructIdentBuilder;

impl FieldIdentBuilder for StructIdentBuilder {
    fn field_join(&self, field: &Ident) -> Ident {
        format_ident!("__FiniteSet_join_field_{}", field)
    }

    fn struct_name(&self) -> TokenStream {
        quote! { Self }
    }

    fn struct_map(&self) -> Ident {
        format_ident!("__FiniteSet_map")
    }
}

/// Field identifier generator for variants in an enum
struct EnumIdentBuilder<'v>(&'v Ident);

impl<'v> EnumIdentBuilder<'v> {
    fn for_variant(variant: &'v Variant) -> Self {
        EnumIdentBuilder(&variant.ident)
    }
}

impl<'v> FieldIdentBuilder for EnumIdentBuilder<'v> {
    fn field_join(&self, field: &Ident) -> Ident {
        format_ident!("__FiniteSet_join_field_{}_{}", self.0, field)
    }

    fn struct_name(&self) -> TokenStream {
        let variant_name = &self.0;
        quote! { Self :: #variant_name }
    }

    fn struct_map(&self) -> Ident {
        format_ident!("__FiniteSet_map_variant_{}", self.0)
    }
}

/// The type of the function used to map recursive tuples to structs
fn struct_map_type(fields: &Fields) -> TokenStream {
    let args = recursive_join(field_types(fields));
    quote! {
        fn(#args) -> Self
    }
}

/// Build a recursive tuple from the identifiers
fn recursive_join(items: impl IntoIterator<Item = impl quote::ToTokens>) -> TokenStream {
    let mut rec_expr = quote! { () };
    for item in items.into_iter() {
        rec_expr = quote! { (#rec_expr, #item) };
    }
    rec_expr
}

/// Get the types of each of the fields in a struct
fn field_types(fields: &Fields) -> impl Iterator<Item = &Type> {
    fields.iter().map(|f| &f.ty)
}
