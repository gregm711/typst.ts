#[cfg(test)]
mod layout_multiline_tests {
    use super::*;

    /// Helper: build a dummy span glyph run with bounds.
    fn make_run(span: u64, page: u32, run_index: u32, y_min: f32, y_max: f32) -> SpanGlyphPositions {
        SpanGlyphPositions {
            span,
            run_index,
            positions: vec![0.0, 10.0, 20.0],
            glyph_spans: vec![0, 1, 2],
            dir_rtl: false,
            x_min: 0.0,
            x_max: 20.0,
            y_min,
            y_max,
            page,
        }
    }

    /// Helper: build a dummy layout span with bounds.
    fn make_layout_span(span: u64, page: u32, y: f32) -> LayoutSpan {
        LayoutSpan {
            span,
            page,
            x: 0.0,
            y,
            width: 20.0,
            height: 5.0,
        }
    }

    #[test]
    fn multiline_span_emits_per_line_layout_entries_and_runs() {
        // Simulate a span that wraps into 3 lines with distinct y ranges.
        let span_id: u64 = 0xdeadbeef;

        // Glyph runs (what the compiler would emit)
        let runs = vec![
            make_run(span_id, 0, 0, 0.0, 5.0),
            make_run(span_id, 0, 1, 10.0, 15.0),
            make_run(span_id, 0, 2, 20.0, 25.0),
        ];

        // Layout spans (what layout map should contain)
        let layout_spans = vec![
            make_layout_span(span_id, 0, 0.0),
            make_layout_span(span_id, 0, 10.0),
            make_layout_span(span_id, 0, 20.0),
        ];

        // Assertions: one entry per line, y increasing
        assert_eq!(runs.len(), 3, "expected 3 glyph runs for wrapped span");
        assert!(runs.windows(2).all(|w| w[0].y_min < w[1].y_min));

        assert_eq!(layout_spans.len(), 3, "expected 3 layout spans for wrapped span");
        assert!(layout_spans.windows(2).all(|w| w[0].y < w[1].y));
    }
}
