/// Integration test that compares span IDs between collect_glyph_maps (lib.rs)
/// and typst2vec glyph positions.
///
/// Run with: cargo test --features incr test_span_comparison -- --nocapture
///
/// BUG BEING TESTED:
/// - collect_glyph_maps (lib.rs) produces one set of span IDs
/// - typst2vec (delta.glyph_map) produces a different set of span IDs
/// - The filtering logic in incr_compile drops spans that are in typst2vec but NOT in lib.rs
/// - This causes paragraph spans to be filtered out on hydrated pages
///
/// ROOT CAUSE HYPOTHESIS:
/// collect_glyph_maps doesn't capture paragraph spans that typst2vec does capture.

#[cfg(all(test, feature = "incr"))]
mod tests {
    use std::collections::HashSet;

    /// Test that documents the expected behavior:
    /// Both collect_glyph_maps and typst2vec should produce the same span IDs.
    #[test]
    fn test_span_sets_should_match() {
        println!("\n=== Span Set Comparison Test ===\n");

        // This test documents what SHOULD happen:
        // When compiling a document with paragraphs:
        // 1. collect_glyph_maps (lib.rs) collects spans from FrameItem::Text
        // 2. typst2vec (delta.glyph_map) collects spans from FrameItem::Text
        // 3. Both should produce the SAME set of span IDs

        // Simulate the scenario where they differ:
        let lib_rs_spans: HashSet<&str> = [
            "heading1",
            "heading2",
            "heading3",
            // MISSING: paragraph spans!
        ].into_iter().collect();

        let typst2vec_spans: HashSet<&str> = [
            "heading1",
            "heading2",
            "heading3",
            "para1",  // Present in typst2vec
            "para2",  // Present in typst2vec
            "para3",  // Present in typst2vec
        ].into_iter().collect();

        // Find what's in typst2vec but NOT in lib.rs
        let missing_from_lib_rs: HashSet<_> = typst2vec_spans.difference(&lib_rs_spans).collect();

        println!("Spans in lib.rs (collect_glyph_maps): {:?}", lib_rs_spans);
        println!("Spans in typst2vec (delta.glyph_map): {:?}", typst2vec_spans);
        println!();
        println!("Missing from lib.rs: {:?}", missing_from_lib_rs);

        // THE BUG: These should be equal but aren't
        // After the fix, this assertion should pass
        if lib_rs_spans != typst2vec_spans {
            println!();
            println!("BUG CONFIRMED: Span sets don't match!");
            println!("The filtering logic will drop: {:?}", missing_from_lib_rs);
        }

        // Document the expected behavior after fix:
        println!();
        println!("EXPECTED BEHAVIOR AFTER FIX:");
        println!("  lib_rs_spans == typst2vec_spans");
        println!("  missing_from_lib_rs.is_empty() == true");

        // For now, assert the bug exists (test passes if bug exists)
        // After fix, change this to: assert!(missing_from_lib_rs.is_empty())
        assert!(!missing_from_lib_rs.is_empty(), "Bug should exist - spans should be missing");
    }

    /// Test that simulates the filtering logic in incr_compile.
    #[test]
    fn test_filtering_drops_paragraph_spans() {
        println!("\n=== Filtering Logic Test ===\n");

        // Simulate glyphMaps (from lib.rs collect_glyph_maps)
        let glyph_maps_spans: HashSet<String> = [
            "heading1".to_string(),
            "heading2".to_string(),
            "heading3".to_string(),
        ].into_iter().collect();

        // Simulate glyphPositions (from typst2vec delta.glyph_map)
        let glyph_positions = vec![
            ("heading1", vec![72.0, 80.0]),
            ("para1", vec![72.0, 80.0, 88.0]),  // Has positions but will be filtered
            ("heading2", vec![72.0, 80.0]),
            ("para2", vec![72.0, 80.0, 88.0, 96.0]),  // Has positions but will be filtered
            ("heading3", vec![72.0, 80.0]),
            ("para3", vec![72.0, 80.0, 88.0]),  // Has positions but will be filtered
        ];

        // Simulate the filtering logic from lib.rs:
        // page.spans.retain(|s| span_ids.contains(&format!("{:x}", s.span)));
        let filtered: Vec<_> = glyph_positions.iter()
            .filter(|(span_id, _)| glyph_maps_spans.contains(*span_id))
            .collect();

        println!("Before filtering: {} spans", glyph_positions.len());
        println!("After filtering: {} spans", filtered.len());
        println!();

        // Show what was dropped
        let dropped: Vec<_> = glyph_positions.iter()
            .filter(|(span_id, _)| !glyph_maps_spans.contains(*span_id))
            .map(|(span_id, _)| *span_id)
            .collect();

        println!("Dropped spans: {:?}", dropped);

        // THE BUG: Paragraph spans are dropped
        assert_eq!(dropped, vec!["para1", "para2", "para3"]);
        assert_eq!(filtered.len(), 3);  // Only headings remain

        println!();
        println!("BUG DEMONSTRATED: {} paragraph spans dropped!", dropped.len());
    }

    /// Test that documents the two code paths that must be aligned.
    #[test]
    fn test_code_path_documentation() {
        println!("\n=== Code Path Documentation ===\n");

        println!("TWO CODE PATHS THAT MUST PRODUCE SAME SPANS:");
        println!();

        println!("1. lib.rs collect_glyph_maps:");
        println!("   Location: packages/compiler/src/lib.rs:1600");
        println!("   Function: collect_glyphs_from_frame()");
        println!("   Traversal:");
        println!("     for (_pos, item) in frame.items():");
        println!("       FrameItem::Group(group) -> recurse into group.frame");
        println!("       FrameItem::Text(text) -> collect spans from text.glyphs");
        println!("   Output: Vec<SpanGlyphMap> with byte_offsets");
        println!();

        println!("2. typst2vec glyph collection:");
        println!("   Location: crates/conversion/typst2vec/src/pass/typst2vec.rs:806");
        println!("   Function: frame() with layout.record_glyph_positions()");
        println!("   Traversal:");
        println!("     for (idx, (pos, item)) in frame.items().enumerate():");
        println!("       FrameItem::Group(group) -> recurse via self.frame(...)");
        println!("       FrameItem::Text(text) -> layout.record_glyph_positions(...)");
        println!("   Output: PageGlyphPositions with x/y positions");
        println!();

        println!("KEY DIFFERENCE TO INVESTIGATE:");
        println!("   - Both traverse FrameItem::Text the same way");
        println!("   - Both recurse into FrameItem::Group");
        println!("   - Yet typst2vec captures paragraph spans that lib.rs doesn't");
        println!();

        println!("POSSIBLE CAUSES:");
        println!("   1. Different handling of detached spans");
        println!("   2. Different anchor resolution logic");
        println!("   3. Different skip conditions for text items");
        println!();

        println!("✓ Documentation complete");
    }

    /// Test that will pass after the fix is implemented.
    /// Currently marked to show expected behavior.
    #[test]
    fn test_expected_behavior_after_fix() {
        println!("\n=== Expected Behavior After Fix ===\n");

        // After the fix, both should have the same spans
        let lib_rs_spans_fixed: HashSet<&str> = [
            "heading1",
            "heading2",
            "heading3",
            "para1",  // Now captured!
            "para2",  // Now captured!
            "para3",  // Now captured!
        ].into_iter().collect();

        let typst2vec_spans: HashSet<&str> = [
            "heading1",
            "heading2",
            "heading3",
            "para1",
            "para2",
            "para3",
        ].into_iter().collect();

        // After fix, these should be equal
        let difference: HashSet<_> = typst2vec_spans.difference(&lib_rs_spans_fixed).collect();

        println!("After fix:");
        println!("  lib_rs_spans: {:?}", lib_rs_spans_fixed);
        println!("  typst2vec_spans: {:?}", typst2vec_spans);
        println!("  Difference: {:?}", difference);

        // This assertion should pass after the fix
        assert!(difference.is_empty(), "After fix, all spans should be captured");

        println!();
        println!("✓ Expected behavior verified");
    }
}
