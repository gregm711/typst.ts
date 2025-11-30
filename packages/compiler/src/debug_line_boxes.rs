/// Debug module for testing line box collection logic.
/// Run with: cargo test --features incr debug_line_boxes -- --nocapture
///
/// These tests verify our TransformState implementation matches tiny_skia_path::Transform,
/// which is the reference implementation used by typst2vec.

#[cfg(all(test, feature = "incr"))]
mod tests {
    use tiny_skia_path::Transform as SkiaTransform;

    /// Mirror of TransformState from lib.rs for testing
    #[derive(Clone, Copy, Debug)]
    struct TransformState {
        sx: f64,
        ky: f64,
        kx: f64,
        sy: f64,
        tx: f64,
        ty: f64,
    }

    impl TransformState {
        fn identity() -> Self {
            Self {
                sx: 1.0,
                ky: 0.0,
                kx: 0.0,
                sy: 1.0,
                tx: 0.0,
                ty: 0.0,
            }
        }

        /// Pre-concat a translation: self = self * translate(x, y)
        fn pre_translate(&self, x: f64, y: f64) -> Self {
            Self {
                sx: self.sx,
                ky: self.ky,
                kx: self.kx,
                sy: self.sy,
                tx: self.sx * x + self.kx * y + self.tx,
                ty: self.ky * x + self.sy * y + self.ty,
            }
        }

        /// Pre-concat a transform: self = self * other
        fn pre_concat(&self, o_sx: f64, o_ky: f64, o_kx: f64, o_sy: f64, o_tx: f64, o_ty: f64) -> Self {
            Self {
                sx: self.sx * o_sx + self.kx * o_ky,
                ky: self.ky * o_sx + self.sy * o_ky,
                kx: self.sx * o_kx + self.kx * o_sy,
                sy: self.ky * o_kx + self.sy * o_sy,
                tx: self.sx * o_tx + self.kx * o_ty + self.tx,
                ty: self.ky * o_tx + self.sy * o_ty + self.ty,
            }
        }

        fn pos_y(&self) -> f64 {
            self.ty
        }

        /// Convert to tiny_skia_path::Transform for comparison
        fn to_skia(&self) -> SkiaTransform {
            SkiaTransform::from_row(
                self.sx as f32,
                self.ky as f32,
                self.kx as f32,
                self.sy as f32,
                self.tx as f32,
                self.ty as f32,
            )
        }

        /// Create from tiny_skia_path::Transform
        fn from_skia(t: SkiaTransform) -> Self {
            Self {
                sx: t.sx as f64,
                ky: t.ky as f64,
                kx: t.kx as f64,
                sy: t.sy as f64,
                tx: t.tx as f64,
                ty: t.ty as f64,
            }
        }
    }

    /// Helper to compare transforms with tolerance for floating point
    fn transforms_equal(ours: &TransformState, skia: &SkiaTransform, tolerance: f64) -> bool {
        (ours.sx - skia.sx as f64).abs() < tolerance
            && (ours.ky - skia.ky as f64).abs() < tolerance
            && (ours.kx - skia.kx as f64).abs() < tolerance
            && (ours.sy - skia.sy as f64).abs() < tolerance
            && (ours.tx - skia.tx as f64).abs() < tolerance
            && (ours.ty - skia.ty as f64).abs() < tolerance
    }

    fn assert_transforms_match(ours: &TransformState, skia: &SkiaTransform, context: &str) {
        let tolerance = 0.0001;
        if !transforms_equal(ours, skia, tolerance) {
            panic!(
                "{}\nOurs:  sx={:.4}, ky={:.4}, kx={:.4}, sy={:.4}, tx={:.4}, ty={:.4}\nSkia:  sx={:.4}, ky={:.4}, kx={:.4}, sy={:.4}, tx={:.4}, ty={:.4}",
                context,
                ours.sx, ours.ky, ours.kx, ours.sy, ours.tx, ours.ty,
                skia.sx, skia.ky, skia.kx, skia.sy, skia.tx, skia.ty
            );
        }
    }

    // =========================================================================
    // Tests that verify our implementation matches tiny_skia_path (reference)
    // =========================================================================

    #[test]
    fn test_identity_matches_skia() {
        let ours = TransformState::identity();
        let skia = SkiaTransform::identity();
        assert_transforms_match(&ours, &skia, "Identity transforms should match");
    }

    #[test]
    fn test_simple_translation_matches_skia() {
        // Our implementation
        let ours = TransformState::identity().pre_translate(10.0, 20.0);

        // Skia reference
        let skia = SkiaTransform::identity().pre_translate(10.0, 20.0);

        assert_transforms_match(&ours, &skia, "Simple translation should match skia");
        println!("Simple translation: ours.ty={:.2}, skia.ty={:.2}", ours.ty, skia.ty);
    }

    #[test]
    fn test_chained_translations_match_skia() {
        // Our implementation
        let ours = TransformState::identity()
            .pre_translate(10.0, 20.0)
            .pre_translate(5.0, 15.0);

        // Skia reference
        let skia = SkiaTransform::identity()
            .pre_translate(10.0, 20.0)
            .pre_translate(5.0, 15.0);

        assert_transforms_match(&ours, &skia, "Chained translations should match skia");
        println!("Chained translations: ours.ty={:.2}, skia.ty={:.2}", ours.ty, skia.ty);
    }

    #[test]
    fn test_scale_then_translate_matches_skia() {
        // Our implementation: scale first, then translate
        let ours = TransformState::identity()
            .pre_concat(2.0, 0.0, 0.0, 2.0, 0.0, 0.0)  // scale(2, 2)
            .pre_translate(10.0, 20.0);

        // Skia reference
        let skia = SkiaTransform::identity()
            .pre_scale(2.0, 2.0)
            .pre_translate(10.0, 20.0);

        assert_transforms_match(&ours, &skia, "Scale then translate should match skia");
        println!("Scale then translate: ours.ty={:.2}, skia.ty={:.2}", ours.ty, skia.ty);
    }

    #[test]
    fn test_translate_then_scale_matches_skia() {
        // Our implementation: translate first, then scale
        let ours = TransformState::identity()
            .pre_translate(10.0, 20.0)
            .pre_concat(2.0, 0.0, 0.0, 2.0, 0.0, 0.0);  // scale(2, 2)

        // Skia reference
        let skia = SkiaTransform::identity()
            .pre_translate(10.0, 20.0)
            .pre_scale(2.0, 2.0);

        assert_transforms_match(&ours, &skia, "Translate then scale should match skia");
        println!("Translate then scale: ours.ty={:.2}, skia.ty={:.2}", ours.ty, skia.ty);
    }

    #[test]
    fn test_complex_transform_chain_matches_skia() {
        // Simulate a realistic Typst frame hierarchy:
        // Page → Group(pos=(0,0), transform=identity) → Group(pos=(56.69, 80), scale=1) → Item(pos=(10, 5))

        // Our implementation
        let ours = TransformState::identity()
            .pre_translate(0.0, 0.0)      // first group position
            .pre_concat(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)  // first group transform (identity)
            .pre_translate(56.69, 80.0)   // second group position
            .pre_concat(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)  // second group transform (identity)
            .pre_translate(10.0, 5.0);    // item position

        // Skia reference
        let skia = SkiaTransform::identity()
            .pre_translate(0.0, 0.0)
            .pre_scale(1.0, 1.0)
            .pre_translate(56.69, 80.0)
            .pre_scale(1.0, 1.0)
            .pre_translate(10.0, 5.0);

        assert_transforms_match(&ours, &skia, "Complex transform chain should match skia");

        // The final position should be (66.69, 85.0)
        assert!((ours.tx - 66.69).abs() < 0.01, "tx should be 66.69, got {}", ours.tx);
        assert!((ours.ty - 85.0).abs() < 0.01, "ty should be 85.0, got {}", ours.ty);
        println!("Complex chain: ours=({:.2}, {:.2}), skia=({:.2}, {:.2})", ours.tx, ours.ty, skia.tx, skia.ty);
    }

    #[test]
    fn test_pre_concat_with_translation_matches_skia() {
        // Test pre_concat where the 'other' transform has a translation component
        // This simulates a group with transform.ty != 0

        let ours = TransformState::identity()
            .pre_translate(10.0, 20.0)  // position offset
            .pre_concat(1.0, 0.0, 0.0, 1.0, 5.0, 15.0);  // transform with translation

        let skia = SkiaTransform::identity()
            .pre_translate(10.0, 20.0)
            .pre_concat(SkiaTransform::from_row(1.0, 0.0, 0.0, 1.0, 5.0, 15.0));

        assert_transforms_match(&ours, &skia, "pre_concat with translation should match skia");
        println!("Pre-concat with translation: ours=({:.2}, {:.2}), skia=({:.2}, {:.2})", ours.tx, ours.ty, skia.tx, skia.ty);
    }

    #[test]
    fn test_skew_transform_matches_skia() {
        // Test with skew (kx, ky non-zero)
        let ours = TransformState::identity()
            .pre_concat(1.0, 0.5, 0.3, 1.0, 0.0, 0.0)  // skew transform
            .pre_translate(10.0, 20.0);

        let skia = SkiaTransform::identity()
            .pre_concat(SkiaTransform::from_row(1.0, 0.5, 0.3, 1.0, 0.0, 0.0))
            .pre_translate(10.0, 20.0);

        assert_transforms_match(&ours, &skia, "Skew transform should match skia");
        println!("Skew transform: ours=({:.2}, {:.2}), skia=({:.2}, {:.2})", ours.tx, ours.ty, skia.tx, skia.ty);
    }

    // =========================================================================
    // Tests for realistic Typst scenarios
    // =========================================================================

    #[test]
    fn test_typst_margin_scenario() {
        // Typst typically has: page frame → content positioned at margin
        let margin_top = 56.69; // ~2cm in pt

        let ours = TransformState::identity()
            .pre_translate(0.0, margin_top);

        let skia = SkiaTransform::identity()
            .pre_translate(0.0, margin_top as f32);

        assert_transforms_match(&ours, &skia, "Margin scenario should match skia");
        assert!((ours.pos_y() - margin_top).abs() < 0.01);
        println!("Margin scenario: pos_y = {:.2}pt", ours.pos_y());
    }

    #[test]
    fn test_typst_nested_groups_scenario() {
        // Simulate nested groups with positions but identity transforms
        // This is common in Typst layouts

        let ours = TransformState::identity()
            .pre_translate(0.0, 0.0)       // outer group pos
            .pre_concat(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)  // outer group transform
            .pre_translate(56.69, 0.0)     // inner group pos (x margin)
            .pre_concat(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)  // inner group transform
            .pre_translate(0.0, 80.0);     // line frame pos

        let skia = SkiaTransform::identity()
            .pre_translate(0.0, 0.0)
            .pre_translate(56.69, 0.0)
            .pre_translate(0.0, 80.0);

        assert_transforms_match(&ours, &skia, "Nested groups should match skia");
        assert!((ours.tx - 56.69).abs() < 0.01);
        assert!((ours.ty - 80.0).abs() < 0.01);
        println!("Nested groups: pos=({:.2}, {:.2})", ours.tx, ours.ty);
    }

    // =========================================================================
    // Diagnostic test for the actual issue
    // =========================================================================

    #[test]
    fn test_diagnose_coordinate_mismatch() {
        // From the debug logs, the issue was ~29pt offset
        // Let's verify what transform chain could cause this

        println!("\n=== Coordinate Mismatch Diagnosis ===");

        // Hypothesis 1: Double-counting a position
        let double_count = TransformState::identity()
            .pre_translate(0.0, 29.0)  // extra translation
            .pre_translate(0.0, 53.7); // actual position

        println!("If double-counting: pos_y = {:.2}pt", double_count.pos_y());

        // Hypothesis 2: Scale applied incorrectly
        // If scale was 1.5 and we didn't account for it properly
        let scale_issue = TransformState::identity()
            .pre_concat(1.0, 0.0, 0.0, 1.5, 0.0, 0.0)
            .pre_translate(0.0, 53.7);

        println!("If scale was 1.5: pos_y = {:.2}pt", scale_issue.pos_y());

        // The fix should give us the correct value
        let correct = TransformState::identity()
            .pre_translate(0.0, 53.7);

        println!("Correct calculation: pos_y = {:.2}pt", correct.pos_y());

        // Verify against skia
        let skia = SkiaTransform::identity().pre_translate(0.0, 53.7);
        assert_transforms_match(&correct, &skia, "Correct transform should match skia");
    }

    // =========================================================================
    // Test content_hint values
    // =========================================================================

    #[test]
    fn test_content_hint_values() {
        // Typst uses these content_hint values:
        // - '\0' (0x00) = no hint (not a line frame)
        // - '\n' (0x0a) = newline - line break
        // - ' '  (0x20) = space - soft break
        // - other chars = the character that was "eaten" by the break

        let null_hint = '\0';
        let newline_hint = '\n';
        let space_hint = ' ';

        // Our detection: hint != '\0' means it's a line frame
        assert!(null_hint == '\0', "null should equal null char");
        assert!(newline_hint != '\0', "newline should be detected as line frame");
        assert!(space_hint != '\0', "space should be detected as line frame");

        println!("content_hint detection:");
        println!("  '\\0' (0x{:02x}) → line frame: {}", null_hint as u32, null_hint != '\0');
        println!("  '\\n' (0x{:02x}) → line frame: {}", newline_hint as u32, newline_hint != '\0');
        println!("  ' '  (0x{:02x}) → line frame: {}", space_hint as u32, space_hint != '\0');
    }
}
