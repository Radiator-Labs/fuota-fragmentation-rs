//! `CozyBoard` External Flash Update Algorithm
//!
//! This library contains the application and bootloader logic for
//! performing a fragmented lorawan FUOTA.
#![no_std]
#![deny(clippy::pedantic)]
// #![deny(clippy::cargo)] // Disabled for ease of development.
#![deny(clippy::alloc_instead_of_core)]
// #![allow(clippy::allow_attributes)] // coming in 1.70.0
#![deny(clippy::allow_attributes_without_reason)]
#![allow(
    clippy::arithmetic_side_effects,
    reason = "Kelvin style guide allows arithmetic_side_effects"
)]
#![allow(
    clippy::as_conversions,
    reason = "Kelvin style guide allows as_conversions"
)]
#![deny(clippy::as_underscore)]
#![deny(clippy::assertions_on_constants)]
#![deny(clippy::clone_on_ref_ptr)]
#![deny(clippy::create_dir)]
#![deny(clippy::dbg_macro)]
#![deny(clippy::decimal_literal_representation)]
#![deny(clippy::default_numeric_fallback)]
#![deny(clippy::default_union_representation)]
#![deny(clippy::deref_by_slicing)]
#![deny(clippy::disallowed_script_idents)]
#![deny(clippy::else_if_without_else)]
#![deny(clippy::empty_drop)]
#![deny(clippy::empty_structs_with_brackets)]
#![allow(
    clippy::exhaustive_enums,
    reason = "Kelvin style guide allows exhaustive_enums"
)]
#![allow(
    clippy::exhaustive_structs,
    reason = "Kelvin style guide allows exhaustive_structs"
)]
#![deny(clippy::exit)]
#![deny(clippy::expect_used)]
#![deny(clippy::filetype_is_file)]
#![deny(clippy::float_arithmetic)]
#![deny(clippy::float_cmp_const)]
#![deny(clippy::fn_to_numeric_cast_any)]
#![deny(clippy::format_push_string)]
#![deny(clippy::get_unwrap)]
#![deny(clippy::if_then_some_else_none)]
#![deny(clippy::impl_trait_in_params)]
#![allow(
    clippy::implicit_return,
    reason = "Kelvin style guide allows implicit_return"
)]
#![deny(clippy::indexing_slicing)] // TODO: Remove this. Due to clippy bug looking into derive macro
#![deny(clippy::inline_asm_x86_att_syntax)]
#![allow(
    clippy::inline_asm_x86_intel_syntax,
    reason = "Kelvin style guide allows inline_asm_x86_intel_syntax"
)]
#![allow(
    clippy::integer_division,
    reason = "Kelvin style guide allows integer division"
)]
#![deny(clippy::large_include_file)]
#![deny(clippy::let_underscore_must_use)]
#![deny(clippy::let_underscore_untyped)]
#![deny(clippy::lossy_float_literal)]
#![deny(clippy::map_err_ignore)]
#![deny(clippy::mem_forget)]
// #![deny(clippy::missing_assert_message)] // coming in 1.70.0
#![allow(
    clippy::missing_docs_in_private_items,
    reason = "Kelvin style guide allows missing_docs_in_private_items"
)]
#![deny(clippy::missing_enforced_import_renames)]
#![allow(
    clippy::missing_inline_in_public_items,
    reason = "Kelvin style guide allows missing_inline_in_public_items"
)]
#![deny(clippy::missing_trait_methods)]
#![deny(clippy::mixed_read_write_in_expression)]
#![deny(clippy::mod_module_files)]
#![allow(
    clippy::module_name_repetitions,
    reason = "Kelvin style guide decision"
)]
#![allow(
    clippy::modulo_arithmetic,
    reason = "Kelvin style guide allows modulo_arithmetic"
)]
#![allow(
    clippy::multiple_crate_versions,
    reason = "Kelvin style guide allows multiple_crate_versions"
)]
#![deny(clippy::multiple_inherent_impl)]
#![allow(
    clippy::multiple_unsafe_ops_per_block,
    reason = "due to: https://github.com/rust-lang/rust-clippy/issues/11312"
)]
#![deny(clippy::mutex_atomic)]
#![deny(clippy::non_ascii_literal)]
#![deny(clippy::panic)]
#![deny(clippy::panic_in_result_fn)]
#![deny(clippy::partial_pub_fields)]
#![deny(clippy::pattern_type_mismatch)]
#![deny(clippy::print_stderr)]
#![deny(clippy::print_stdout)]
#![deny(clippy::pub_use)]
#![allow(
    clippy::question_mark_used,
    reason = "Kelvin style guide allows question_mark_used"
)]
#![deny(clippy::rc_buffer)]
#![deny(clippy::rc_mutex)]
#![allow(
    clippy::redundant_feature_names,
    reason = "Kelvin style guide allows redundant_feature_names"
)]
// #![deny(clippy::ref_patterns)] // coming in 1.70.0
#![deny(clippy::rest_pat_in_fully_bound_structs)]
#![deny(clippy::same_name_method)]
#![allow(
    clippy::self_named_module_files,
    reason = "Kelvin style guide allows self_named_module_files"
)]
#![deny(clippy::semicolon_outside_block)]
#![allow(
    clippy::separated_literal_suffix,
    reason = "Kelvin style guide allows separated_literal_suffix"
)]
#![deny(clippy::shadow_reuse)]
#![deny(clippy::shadow_same)]
#![deny(clippy::shadow_unrelated)]
#![allow(
    clippy::single_char_lifetime_names,
    reason = "Kelvin style guide allows single_char_lifetime_names"
)]
#![deny(clippy::std_instead_of_alloc)]
#![deny(clippy::std_instead_of_core)]
#![deny(clippy::str_to_string)]
#![deny(clippy::string_add)]
#![deny(clippy::string_slice)]
#![deny(clippy::string_to_string)]
#![deny(clippy::suspicious_xor_used_as_pow)]
//#![deny(clippy::tests_outside_test_module)] // coming in 1.70.0
#![deny(clippy::todo)]
#![deny(clippy::try_err)]
#![deny(clippy::undocumented_unsafe_blocks)]
#![deny(clippy::unimplemented)]
#![deny(clippy::unnecessary_safety_comment)]
#![deny(clippy::unnecessary_safety_doc)]
#![deny(clippy::unnecessary_self_imports)]
#![deny(clippy::unneeded_field_pattern)]
#![deny(clippy::unreachable)]
#![deny(clippy::unseparated_literal_suffix)]
#![deny(clippy::unwrap_in_result)]
#![deny(clippy::unwrap_used)]
#![deny(clippy::use_debug)]
#![deny(clippy::verbose_file_reads)]
#![deny(clippy::wildcard_enum_match_arm)]

pub(crate) mod bitcache;
pub mod fragmentation;
pub mod manager;
pub mod protocol;
pub mod ring;
pub mod spi_flash;

#[cfg(test)]
mod tests;
#[cfg(any(test, feature = "testutils"))]
pub mod testutils;
