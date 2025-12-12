/// Parity tests for hydration glyph data under incr-glyph-maps.
///
/// These are synthetic (no full Typst world) but lock down the
/// "single-source by construction" contract:
/// glyphMaps are fused from typst2vec glyph_offsets, then glyphPositions
/// and layoutMap are filtered by the same span-id set.

#[cfg(all(test, feature = "incr", feature = "incr-glyph-maps", not(target_arch = "wasm32")))]
mod tests {
    use std::collections::HashSet;

    use crate::fuse_glyph_maps_from_offsets;
    use reflexo_typst2vec::layout::{
        PageGlyphOffsets, PageGlyphPositions, PageLayout, SpanBBox, SpanGlyphOffsets,
        SpanGlyphPositions,
    };

    #[test]
    fn test_fuse_skips_unresolvable_spans_and_merges_runs() {
        let span_ranges = vec![
            ("1000".to_string(), 0, 100usize, 110usize),
            ("2000".to_string(), 0, 200usize, 210usize),
        ];

        let glyph_offsets_pages = vec![PageGlyphOffsets {
            page: 0,
            spans: vec![
                SpanGlyphOffsets {
                    span: 0x1000,
                    run_index: 0,
                    offsets: vec![0, 1, 2],
                },
                SpanGlyphOffsets {
                    span: 0x1000,
                    run_index: 1,
                    offsets: vec![3, 4],
                },
                // Not in span_ranges -> should be skipped.
                SpanGlyphOffsets {
                    span: 0x9999,
                    run_index: 0,
                    offsets: vec![0, 1],
                },
            ],
        }];

        let fused = fuse_glyph_maps_from_offsets(glyph_offsets_pages, &span_ranges);
        assert_eq!(fused.len(), 1);
        assert_eq!(fused[0].page, 0);

        let span_1000 = fused[0]
            .spans
            .iter()
            .find(|s| s.span_id == "1000")
            .expect("expected fused span 1000");
        assert_eq!(span_1000.byte_offsets, vec![100, 101, 102, 103, 104]);

        assert!(
            fused[0].spans.iter().all(|s| s.span_id != "9999"),
            "unresolvable spans must be skipped"
        );
    }

    #[test]
    fn test_filtered_span_sets_match_across_gm_gp_lm() {
        let span_ranges = vec![("1000".to_string(), 0, 10usize, 20usize)];
        let glyph_offsets_pages = vec![PageGlyphOffsets {
            page: 0,
            spans: vec![SpanGlyphOffsets {
                span: 0x1000,
                run_index: 0,
                offsets: vec![0, 1, 2],
            }],
        }];
        let glyph_maps = fuse_glyph_maps_from_offsets(glyph_offsets_pages, &span_ranges);

        let span_ids: HashSet<u64> = glyph_maps
            .iter()
            .flat_map(|p| {
                p.spans
                    .iter()
                    .filter_map(|s| u64::from_str_radix(&s.span_id, 16).ok())
            })
            .collect();

        // Build glyph positions/layout maps with an extra unresolvable span.
        let glyph_positions = vec![PageGlyphPositions {
            page: 0,
            spans: vec![
                SpanGlyphPositions {
                    span: 0x1000,
                    run_index: 0,
                    positions: vec![0.0, 1.0],
                    glyph_spans: vec![0x1000, 0x1000],
                    dir_rtl: false,
                    x_min: 0.0,
                    x_max: 1.0,
                    y_min: 0.0,
                    y_max: 1.0,
                },
                SpanGlyphPositions {
                    span: 0x9999,
                    run_index: 0,
                    positions: vec![0.0, 1.0],
                    glyph_spans: vec![0x9999, 0x9999],
                    dir_rtl: false,
                    x_min: 0.0,
                    x_max: 1.0,
                    y_min: 0.0,
                    y_max: 1.0,
                },
            ],
        }];

        let layout_map = vec![PageLayout {
            page: 0,
            spans: vec![
                SpanBBox {
                    span: 0x1000,
                    x: 0.0,
                    y: 0.0,
                    width: 10.0,
                    height: 10.0,
                },
                SpanBBox {
                    span: 0x9999,
                    x: 0.0,
                    y: 0.0,
                    width: 10.0,
                    height: 10.0,
                },
            ],
        }];

        let filtered_gp: Vec<_> = glyph_positions
            .into_iter()
            .map(|mut p| {
                p.spans.retain(|s| span_ids.contains(&s.span));
                p
            })
            .collect();
        let filtered_lm: Vec<_> = layout_map
            .into_iter()
            .map(|mut p| {
                p.spans.retain(|s| span_ids.contains(&s.span));
                p
            })
            .collect();

        let gm_spans: HashSet<String> = glyph_maps
            .iter()
            .flat_map(|p| p.spans.iter().map(|s| s.span_id.clone()))
            .collect();
        let gp_spans: HashSet<String> = filtered_gp
            .iter()
            .flat_map(|p| p.spans.iter().map(|s| format!("{:x}", s.span)))
            .collect();
        let lm_spans: HashSet<String> = filtered_lm
            .iter()
            .flat_map(|p| p.spans.iter().map(|s| format!("{:x}", s.span)))
            .collect();

        assert_eq!(gm_spans, gp_spans);
        assert_eq!(gm_spans, lm_spans);
        assert_eq!(gm_spans, HashSet::from(["1000".to_string()]));
    }
}

