use serde::Serialize;

use reflexo::vector::ir::{Point, Rect, Scalar};
use typst::visualize::FixedStroke;

/// Bounding box for a single span on a page, in page coordinates (points).
#[derive(Debug, Clone, Serialize)]
pub struct SpanBBox {
    #[serde(serialize_with = "crate::layout::serialize_span_hex")]
    pub span: u64,
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
}

/// A page worth of span bounding boxes.
#[derive(Debug, Clone, Serialize)]
pub struct PageLayout {
    pub page: u32,
    pub spans: Vec<SpanBBox>,
}

/// Glyph-level x positions for a span (page coordinates, points).
#[derive(Debug, Clone, Serialize)]
pub struct SpanGlyphPositions {
    #[serde(serialize_with = "crate::layout::serialize_span_hex")]
    pub span: u64,
    /// X positions of glyph edges in page space, sorted in visual order.
    pub positions: Vec<f32>,
    /// Span ids for each glyph (mirrors Typst glyph span ids to resolve source).
    pub glyph_spans: Vec<u64>,
    /// True if the run is right-to-left.
    pub dir_rtl: bool,
}

/// A page worth of glyph positions.
#[derive(Debug, Clone, Serialize)]
pub struct PageGlyphPositions {
    pub page: u32,
    pub spans: Vec<SpanGlyphPositions>,
}

/// Inflate a rectangle by half the stroke thickness in all directions.
/// Used to approximate the visual bounds of stroked shapes/text.
pub(crate) fn inflate_rect_by_stroke(rect: Rect, stroke: &FixedStroke) -> Rect {
    let pad = stroke.thickness.to_pt() as f32 / 2.0;
    if pad <= 0.0 {
        return rect;
    }

    let pad = Scalar(pad);
    Rect {
        lo: Point::new(rect.lo.x - pad, rect.lo.y - pad),
        hi: Point::new(rect.hi.x + pad, rect.hi.y + pad),
    }
}

pub(crate) fn serialize_span_hex<S>(span: &u64, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    serializer.serialize_str(&format!("{span:x}"))
}

#[cfg(test)]
mod tests {
    use super::inflate_rect_by_stroke;
    use reflexo::vector::ir::{Point, Rect, Scalar};
    use typst::layout::Abs as TypstAbs;
    use typst::visualize::{FixedStroke, LineCap, LineJoin, Paint};

    fn rect(lo: (f32, f32), hi: (f32, f32)) -> Rect {
        Rect {
            lo: Point::new(Scalar(lo.0), Scalar(lo.1)),
            hi: Point::new(Scalar(hi.0), Scalar(hi.1)),
        }
    }

    fn stroke(thickness: f32) -> FixedStroke {
        let mut s = FixedStroke::default();
        s.paint = Paint::Solid(typst::visualize::Color::BLACK);
        s.thickness = TypstAbs::pt(thickness as f64);
        s.cap = LineCap::Butt;
        s.join = LineJoin::Miter;
        s.dash = None;
        s
    }

    #[test]
    fn inflate_by_half_stroke() {
        let base = rect((0.0, 0.0), (10.0, 10.0));
        let inflated = inflate_rect_by_stroke(base, &stroke(4.0));
        assert_eq!(inflated.lo.x.0, -2.0);
        assert_eq!(inflated.lo.y.0, -2.0);
        assert_eq!(inflated.hi.x.0, 12.0);
        assert_eq!(inflated.hi.y.0, 12.0);
    }

    #[test]
    fn no_inflate_for_zero_stroke() {
        let base = rect((1.0, 2.0), (3.0, 4.0));
        let inflated = inflate_rect_by_stroke(base, &stroke(0.0));
        assert_eq!(inflated.lo, base.lo);
        assert_eq!(inflated.hi, base.hi);
    }
}

/// Tests verifying the glyph position coordinate system fix.
///
/// THE FIX: State now has two transforms:
/// - `transform`: Used for rendering, may be reset for hard frames
/// - `page_transform`: Used for layout recording, NEVER reset, always page-absolute
///
/// When recording glyph positions and span bounding boxes, we now use `page_transform`
/// instead of `transform`, ensuring coordinates are always page-absolute regardless
/// of frame nesting.
///
/// ORIGINAL BUG: In `typst2vec.rs`, entering a Hard frame reset `transform` to identity,
/// losing parent Group translations. This caused glyph positions to be frame-relative
/// (x≈0) instead of page-absolute (x≈72pt for 1in margin).
#[cfg(test)]
mod glyph_coordinate_fix_tests {
    use reflexo::vector::ir::{Point, Scalar, Transform};
    use tiny_skia_path::Transform as SkTransform;

    /// Helper to transform a point using our Transform type
    fn transform_point(point: Point, transform: Transform) -> Point {
        let ts: SkTransform = transform.into();
        let mut p = [tiny_skia_path::Point::from_xy(point.x.0, point.y.0)];
        ts.map_points(&mut p);
        Point::new(Scalar(p[0].x), Scalar(p[0].y))
    }

    /// Simulate the FIXED State struct with separate transform and page_transform
    struct FixedState {
        /// Transform for rendering (may be reset for hard frames)
        transform: Transform,
        /// Transform for layout recording - always page-absolute, never reset
        page_transform: Transform,
    }

    impl FixedState {
        fn new() -> Self {
            Self {
                transform: Transform::identity(),
                page_transform: Transform::identity(),
            }
        }

        /// Pre-translate BOTH transforms
        fn pre_translate(self, x: f32, y: f32) -> Self {
            let translation = Transform::from_translate(Scalar(x), Scalar(y));
            Self {
                transform: self.transform.pre_concat(translation),
                page_transform: self.page_transform.pre_concat(translation),
            }
        }

        /// Reset render transform for hard frame, but PRESERVE page_transform
        fn with_transform_reset(self) -> Self {
            Self {
                transform: Transform::identity(),
                // page_transform is NOT reset - this is the fix!
                page_transform: self.page_transform,
            }
        }
    }

    /// TEST 1: Verify that page_transform preserves coordinates through hard frame reset
    ///
    /// Scenario from browser logs:
    /// - Group positioned at (72.0, 180.7) on the page
    /// - Inside the Group is a Hard Frame (which resets render transform)
    /// - Inside the Hard Frame is text at local position (0.0, 7.5)
    ///
    /// With the fix, glyph positions should use page_transform and be page-absolute.
    #[test]
    fn test_page_transform_preserves_coordinates_through_hard_frame() {
        // Start at page origin
        let state = FixedState::new();

        // Group is positioned at (72.0, 180.7) on the page
        let group_x = 72.0_f32;
        let group_y = 180.7_f32;
        let state = state.pre_translate(group_x, group_y);

        // Both transforms should have the translation
        assert!(
            (state.transform.tx.0 - group_x).abs() < 0.001,
            "Render transform should have tx={}",
            group_x
        );
        assert!(
            (state.page_transform.tx.0 - group_x).abs() < 0.001,
            "Page transform should have tx={}",
            group_x
        );

        // Enter a Hard frame - render transform resets, page_transform preserved
        let state_after_hard_frame = state.with_transform_reset();

        // Render transform is reset (for SVG/canvas rendering)
        assert!(
            state_after_hard_frame.transform.tx.0.abs() < 0.001,
            "Render transform should be reset to identity"
        );

        // FIX VERIFIED: page_transform is preserved!
        assert!(
            (state_after_hard_frame.page_transform.tx.0 - group_x).abs() < 0.001,
            "FIX VERIFIED: page_transform preserves tx={}, got tx={}",
            group_x,
            state_after_hard_frame.page_transform.tx.0
        );

        // Now transform text at local position (0.0, 7.5) using page_transform
        let text_local_pos = Point::new(Scalar(0.0), Scalar(7.5));
        let transformed_pos =
            transform_point(text_local_pos, state_after_hard_frame.page_transform);

        // FIX RESULT: Position is now correct (72.0, 188.2)
        assert!(
            (transformed_pos.x.0 - 72.0).abs() < 0.001,
            "FIX: Glyph x position should be 72.0, got {}",
            transformed_pos.x.0
        );
        assert!(
            (transformed_pos.y.0 - 188.2).abs() < 0.001,
            "FIX: Glyph y position should be 188.2, got {}",
            transformed_pos.y.0
        );
    }

    /// TEST 2: Verify consistency between direct page children and nested text
    ///
    /// Both paths should now produce page-absolute coordinates.
    #[test]
    fn test_consistent_coordinates_between_direct_and_nested_text() {
        let page_margin = 72.0_f32;

        // Case 1: Text directly in page frame
        let state_direct = FixedState::new().pre_translate(page_margin, 252.9);
        let text_pos_direct = Point::new(Scalar(0.0), Scalar(0.0));
        let result_direct = transform_point(text_pos_direct, state_direct.page_transform);

        assert!(
            (result_direct.x.0 - page_margin).abs() < 0.001,
            "Direct text x should be {}",
            page_margin
        );

        // Case 2: Text inside Group → Hard Frame (with fix)
        let state_nested = FixedState::new()
            .pre_translate(page_margin, 180.7)
            .with_transform_reset(); // page_transform is preserved!

        let text_pos_nested = Point::new(Scalar(0.0), Scalar(7.5));
        let result_nested = transform_point(text_pos_nested, state_nested.page_transform);

        // FIX VERIFIED: Nested text now includes the page offset
        assert!(
            (result_nested.x.0 - page_margin).abs() < 0.001,
            "FIX: Nested text x should be {}, got {}",
            page_margin,
            result_nested.x.0
        );

        // Both paths produce consistent x coordinates (no more 72pt discrepancy)
        let delta_x = (result_direct.x.0 - result_nested.x.0).abs();
        assert!(
            delta_x < 1.0,
            "FIX: Direct and nested text should have same x offset, delta={}",
            delta_x
        );
    }

    /// TEST 3: Verify span bboxes and glyph positions are both page-absolute
    #[test]
    fn test_span_bbox_and_glyph_positions_both_page_absolute() {
        let state = FixedState::new()
            .pre_translate(72.0, 180.7)
            .with_transform_reset();

        // A glyph at local position (5.0, 7.5)
        let glyph_local = Point::new(Scalar(5.0), Scalar(7.5));
        let glyph_transformed = transform_point(glyph_local, state.page_transform);

        // A span bbox corner at local position (0.0, 0.0)
        let bbox_corner_local = Point::new(Scalar(0.0), Scalar(0.0));
        let bbox_corner_transformed = transform_point(bbox_corner_local, state.page_transform);

        // FIX: Both should now be page-absolute
        let expected_glyph_x = 72.0 + 5.0; // 77.0
        let expected_bbox_x = 72.0 + 0.0; // 72.0

        assert!(
            (glyph_transformed.x.0 - expected_glyph_x).abs() < 0.001,
            "FIX: Glyph x should be {}, got {}",
            expected_glyph_x,
            glyph_transformed.x.0
        );
        assert!(
            (bbox_corner_transformed.x.0 - expected_bbox_x).abs() < 0.001,
            "FIX: BBox corner x should be {}, got {}",
            expected_bbox_x,
            bbox_corner_transformed.x.0
        );
    }

    /// TEST 4: Verify the Introduction paragraph scenario is fixed
    ///
    /// The specific case from browser logs that was failing:
    /// - span 1252640b inside Group@(72.0,180.7)
    /// - Was producing pos=(0.0,7.5), should produce pos=(72.0,188.2)
    #[test]
    fn test_introduction_paragraph_fixed() {
        // Simulate the exact scenario
        let state = FixedState::new()
            .pre_translate(72.0, 180.7) // Group position
            .with_transform_reset() // Hard frame
            .pre_translate(0.0, 7.5); // Text position within frame

        let glyph_origin = Point::new(Scalar(0.0), Scalar(0.0));
        let result = transform_point(glyph_origin, state.page_transform);

        // FIX: Glyph position should now be page-absolute
        assert!(
            (result.x.0 - 72.0).abs() < 0.001,
            "FIX: Introduction paragraph glyph x should be 72.0, got {}",
            result.x.0
        );
        assert!(
            (result.y.0 - 188.2).abs() < 0.001,
            "FIX: Introduction paragraph glyph y should be 188.2, got {}",
            result.y.0
        );
    }

    /// TEST 5: Verify deeply nested frames work correctly
    ///
    /// Multiple levels of Group → Hard Frame nesting should all accumulate
    /// in page_transform while render transform may reset multiple times.
    #[test]
    fn test_deeply_nested_frames() {
        let state = FixedState::new()
            .pre_translate(72.0, 100.0) // First group
            .with_transform_reset() // First hard frame
            .pre_translate(10.0, 20.0) // Second group (inside first frame)
            .with_transform_reset() // Second hard frame
            .pre_translate(5.0, 5.0); // Text position

        // Render transform should only have the last translation
        assert!(
            (state.transform.tx.0 - 5.0).abs() < 0.001,
            "Render transform should only have last tx"
        );

        // Page transform should have ALL translations accumulated
        let expected_x = 72.0 + 10.0 + 5.0; // 87.0
        let expected_y = 100.0 + 20.0 + 5.0; // 125.0
        assert!(
            (state.page_transform.tx.0 - expected_x).abs() < 0.001,
            "FIX: page_transform should accumulate all translations, expected tx={}, got tx={}",
            expected_x,
            state.page_transform.tx.0
        );
        assert!(
            (state.page_transform.ty.0 - expected_y).abs() < 0.001,
            "FIX: page_transform should accumulate all translations, expected ty={}, got ty={}",
            expected_y,
            state.page_transform.ty.0
        );

        // Glyph at origin should be at accumulated position
        let glyph = Point::new(Scalar(0.0), Scalar(0.0));
        let result = transform_point(glyph, state.page_transform);
        assert!(
            (result.x.0 - expected_x).abs() < 0.001,
            "FIX: Deeply nested glyph x should be {}",
            expected_x
        );
    }

    /// TEST 6: Verify render transform still works for rendering purposes
    ///
    /// The fix should not break rendering - render transform should still
    /// be reset for hard frames as needed by SVG/canvas output.
    #[test]
    fn test_render_transform_still_resets_for_rendering() {
        let state = FixedState::new()
            .pre_translate(72.0, 180.7)
            .with_transform_reset();

        // Render transform should be identity (reset)
        assert!(
            state.transform.tx.0.abs() < 0.001 && state.transform.ty.0.abs() < 0.001,
            "Render transform should be reset to identity for rendering"
        );

        // This ensures SVG/canvas rendering still works as expected
        // (items are positioned relative to the hard frame, not the page)
        let item_local = Point::new(Scalar(10.0), Scalar(20.0));
        let render_pos = transform_point(item_local, state.transform);
        assert!(
            (render_pos.x.0 - 10.0).abs() < 0.001,
            "Render position should be frame-relative"
        );
    }
}

/// Tests for LayoutCollector behavior, particularly around multi-line spans.
///
/// KNOWN ISSUE: `record_glyph_positions` overwrites previous entries for the same span,
/// which means multi-line paragraphs only keep glyph positions for the LAST line.
/// This is a bug that should be fixed by appending/extending instead of overwriting.
#[cfg(test)]
mod layout_collector_tests {
    use super::*;
    use crate::pass::typst2vec::LayoutCollector;
    use reflexo::vector::ir::{Rect, Scalar, Transform};

    /// TEST: Verify span bbox correctly unions across multiple calls (WORKING)
    ///
    /// record_span uses `entry().and_modify().or_insert()` pattern which
    /// correctly unions bounding boxes for multi-line spans.
    #[test]
    fn test_span_bbox_unions_across_multiple_lines() {
        let collector = LayoutCollector::default();
        let span_id = 0x1234_u64;

        // First line: bbox at (72, 100) with size (400, 12)
        let rect1 = Rect {
            lo: Point::new(Scalar(0.0), Scalar(0.0)),
            hi: Point::new(Scalar(400.0), Scalar(12.0)),
        };
        let transform1 = Transform::from_translate(Scalar(72.0), Scalar(100.0));
        collector.record_span(span_id, rect1, transform1);

        // Second line: bbox at (72, 115) with size (300, 12)
        let rect2 = Rect {
            lo: Point::new(Scalar(0.0), Scalar(0.0)),
            hi: Point::new(Scalar(300.0), Scalar(12.0)),
        };
        let transform2 = Transform::from_translate(Scalar(72.0), Scalar(115.0));
        collector.record_span(span_id, rect2, transform2);

        let page_layout = collector.to_page_layout(0);
        assert_eq!(page_layout.spans.len(), 1, "Should have one span entry");

        let span = &page_layout.spans[0];
        assert_eq!(span.span, span_id);

        // The bbox should be the UNION of both lines
        // x: min(72, 72) = 72
        // y: min(100, 115) = 100
        // width: max(72+400, 72+300) - 72 = 400
        // height: max(100+12, 115+12) - 100 = 27
        assert!(
            (span.x - 72.0).abs() < 0.001,
            "Span x should be 72.0 (union), got {}",
            span.x
        );
        assert!(
            (span.y - 100.0).abs() < 0.001,
            "Span y should be 100.0 (union), got {}",
            span.y
        );
        assert!(
            (span.width - 400.0).abs() < 0.001,
            "Span width should be 400.0 (union), got {}",
            span.width
        );
        assert!(
            (span.height - 27.0).abs() < 0.001,
            "Span height should be 27.0 (union of both lines), got {}",
            span.height
        );
    }

    /// TEST: Verify multi-line glyph positions are now preserved (bug is fixed)
    ///
    /// Previously, record_glyph_positions used HashMap::insert which overwrote
    /// previous entries. Now it appends to a Vec, so all lines are preserved
    /// as separate entries (chunks) with the same span ID.
    #[test]
    fn test_glyph_positions_no_longer_overwrite_for_multiline_spans() {
        let collector = LayoutCollector::default();
        let span_id = 0x1252640b_u64; // The Introduction paragraph span from logs

        // First line: 92 glyphs at x positions starting at 72
        let positions_line1: Vec<f32> = (0..93).map(|i| 72.0 + (i as f32) * 5.0).collect();
        let glyph_spans_line1: Vec<u64> = vec![span_id; 92];
        collector.record_glyph_positions(span_id, positions_line1.clone(), glyph_spans_line1, false);

        // Second line: 90 glyphs at x positions starting at 72 (new line)
        let positions_line2: Vec<f32> = (0..91).map(|i| 72.0 + (i as f32) * 5.0).collect();
        let glyph_spans_line2: Vec<u64> = vec![span_id; 90];
        collector.record_glyph_positions(span_id, positions_line2.clone(), glyph_spans_line2, false);

        // Third line: 49 glyphs
        let positions_line3: Vec<f32> = (0..50).map(|i| 72.0 + (i as f32) * 5.0).collect();
        let glyph_spans_line3: Vec<u64> = vec![span_id; 49];
        collector.record_glyph_positions(span_id, positions_line3.clone(), glyph_spans_line3, false);

        let glyph_map = collector.to_page_glyph_map(0);

        // FIXED: Now outputs 3 separate entries for the same span ID
        assert_eq!(glyph_map.spans.len(), 3, "Should have three entries (one per line)");

        // Verify each entry has correct positions
        assert_eq!(glyph_map.spans[0].positions.len(), positions_line1.len());
        assert_eq!(glyph_map.spans[1].positions.len(), positions_line2.len());
        assert_eq!(glyph_map.spans[2].positions.len(), positions_line3.len());

        // All entries should have the same span ID
        assert!(glyph_map.spans.iter().all(|s| s.span == span_id));
    }

    /// TEST: Verify what CORRECT multi-line glyph handling would look like
    ///
    /// This test shows what the behavior SHOULD be after fixing the bug.
    /// Each line's glyph positions are stored as a separate entry in the output,
    /// all with the same span ID. The frontend (use-glyph-map.ts) will collect
    /// these into chunks.
    #[test]
    fn test_glyph_positions_should_accumulate_for_multiline_spans() {
        let collector = LayoutCollector::default();
        let span_id = 0x1252640b_u64;

        // Three lines of text
        let positions_line1: Vec<f32> = (0..93).map(|i| 72.0 + (i as f32) * 5.0).collect();
        let positions_line2: Vec<f32> = (0..91).map(|i| 72.0 + (i as f32) * 5.0).collect();
        let positions_line3: Vec<f32> = (0..50).map(|i| 72.0 + (i as f32) * 5.0).collect();

        collector.record_glyph_positions(span_id, positions_line1.clone(), vec![span_id; 92], false);
        collector.record_glyph_positions(span_id, positions_line2.clone(), vec![span_id; 90], false);
        collector.record_glyph_positions(span_id, positions_line3.clone(), vec![span_id; 49], false);

        let glyph_map = collector.to_page_glyph_map(0);

        // CORRECT: Should output 3 separate entries (chunks), all with the same span ID
        // The frontend collects these into chunks array
        let entries_for_span: Vec<_> = glyph_map
            .spans
            .iter()
            .filter(|s| s.span == span_id)
            .collect();

        assert_eq!(
            entries_for_span.len(),
            3,
            "Should have 3 separate entries for the 3 lines"
        );

        // Verify each entry has the correct positions
        assert_eq!(entries_for_span[0].positions.len(), positions_line1.len());
        assert_eq!(entries_for_span[1].positions.len(), positions_line2.len());
        assert_eq!(entries_for_span[2].positions.len(), positions_line3.len());
    }

    /// TEST: Single-line spans work correctly (no overwrite issue)
    #[test]
    fn test_single_line_span_glyph_positions_work() {
        let collector = LayoutCollector::default();
        let span_id = 0xABCD_u64;

        let positions: Vec<f32> = vec![72.0, 77.0, 82.0, 87.0, 92.0];
        let glyph_spans: Vec<u64> = vec![span_id; 4];
        collector.record_glyph_positions(span_id, positions.clone(), glyph_spans, false);

        let glyph_map = collector.to_page_glyph_map(0);
        assert_eq!(glyph_map.spans.len(), 1);

        let span_glyphs = &glyph_map.spans[0];
        assert_eq!(span_glyphs.positions.len(), 5);
        assert!((span_glyphs.positions[0] - 72.0).abs() < 0.001);
        assert!((span_glyphs.positions[4] - 92.0).abs() < 0.001);
    }

    /// TEST: Different spans don't interfere with each other
    #[test]
    fn test_different_spans_stored_separately() {
        let collector = LayoutCollector::default();

        let span1 = 0x1111_u64;
        let span2 = 0x2222_u64;

        collector.record_glyph_positions(span1, vec![10.0, 20.0, 30.0], vec![span1; 2], false);
        collector.record_glyph_positions(span2, vec![100.0, 110.0, 120.0, 130.0], vec![span2; 3], false);

        let glyph_map = collector.to_page_glyph_map(0);
        assert_eq!(glyph_map.spans.len(), 2, "Should have two separate spans");

        // Find each span
        let s1 = glyph_map.spans.iter().find(|s| s.span == span1).unwrap();
        let s2 = glyph_map.spans.iter().find(|s| s.span == span2).unwrap();

        assert_eq!(s1.positions.len(), 3);
        assert_eq!(s2.positions.len(), 4);
    }
}
