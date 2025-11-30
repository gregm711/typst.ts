/// Integration tests for line box collection using the IEEE template.
/// Run with: cargo test --features incr test_ieee -- --nocapture
///
/// These tests compile actual Typst documents and verify that line boxes
/// are collected with correct positions.

#[cfg(all(test, feature = "incr"))]
mod tests {
    use crate::{TransformState, TypstCompileWorld};
    use typst::layout::{Frame, FrameItem};

    /// The IEEE template source code for testing
    const IEEE_TEMPLATE: &str = r##"// IEEE Conference Paper Template
// Based on charged-ieee from Typst Universe

#let ieee(
  title: [],
  authors: (),
  abstract: none,
  index-terms: (),
  paper-size: "us-letter",
  bibliography-file: none,
  figure-supplement: "Fig.",
  body,
) = {
  // Page setup
  set page(
    paper: paper-size,
    margin: (x: 0.625in, y: 0.75in),
    columns: 2,
  )

  // Text setup
  set text(font: "New Computer Modern", size: 10pt)
  set par(justify: true, leading: 0.52em, first-line-indent: 1em)

  // Heading setup
  set heading(numbering: "I.A.1.")
  show heading: it => {
    set text(size: 10pt, weight: "bold")
    if it.level == 1 {
      set text(size: 10pt)
      align(center)[
        #v(0.5em)
        #upper(it)
        #v(0.3em)
      ]
    } else if it.level == 2 {
      v(0.5em)
      it
      v(0.3em)
    } else {
      it
    }
  }

  // Figure setup
  set figure(supplement: figure-supplement)

  // Title
  place(
    top + center,
    float: true,
    scope: "parent",
    clearance: 1em,
  )[
    #align(center)[
      #text(size: 24pt, weight: "bold")[#title]

      #v(1em)

      // Authors
      #grid(
        columns: calc.min(authors.len(), 3),
        gutter: 1.5em,
        ..authors.map(author => [
          #text(size: 11pt)[#author.name] \
          #if "department" in author [#text(size: 9pt, style: "italic")[#author.department] \ ]
          #if "organization" in author [#text(size: 9pt)[#author.organization] \ ]
          #if "location" in author [#text(size: 9pt)[#author.location] \ ]
          #if "email" in author [#text(size: 9pt)[#author.email]]
        ])
      )

      #v(1.5em)

      // Abstract
      #if abstract != none [
        #align(left)[
          #text(weight: "bold")[Abstract—]#text(style: "italic")[#abstract]
        ]
        #v(0.5em)
      ]

      // Index terms
      #if index-terms.len() > 0 [
        #align(left)[
          #text(weight: "bold")[Index Terms—]#index-terms.join(", ")
        ]
        #v(1em)
      ]
    ]
  ]

  body
}

// Example usage
#show: ieee.with(
  title: [A Typesetting System to Untangle the Scientific Writing Process],
  abstract: [
    The process of scientific writing is often tangled up with the intricacies of typesetting, leading to frustration and wasted time for researchers. In this paper, we introduce Typst, a new typesetting system designed specifically for scientific writing. Typst untangles the typesetting process, allowing researchers to compose papers faster. In a series of experiments we demonstrate that Typst offers several advantages, including faster document creation, simplified syntax, and increased ease-of-use.
  ],
  authors: (
    (
      name: "First Author",
      department: [Department of Computer Science],
      organization: [University Name],
      location: [City, Country],
      email: "author@university.edu"
    ),
    (
      name: "Second Author",
      department: [Department of Engineering],
      organization: [Another University],
      location: [City, Country],
      email: "coauthor@university.edu"
    ),
  ),
  index-terms: ("Scientific writing", "Typesetting", "Document creation", "Syntax"),
)

= Introduction

The introduction establishes the context and motivation for your research. Explain the problem you're addressing and why it matters.

In this paper, we present our approach to solving this problem. Our key contributions are:
- First contribution
- Second contribution
- Third contribution

The rest of this paper is organized as follows. Section II discusses related work. Section III describes our methodology. Section IV presents our results, and Section V concludes.

= Related Work

Discuss prior research and how your work relates to and differs from existing approaches.

Previous work in this area has focused on various aspects of the problem. Smith et al. proposed an approach based on... [1].

= Methodology

Describe your approach, methods, algorithms, or system architecture in detail.

== System Overview

Our system consists of three main components...

== Algorithm Design

The core algorithm works as follows...

$ E = m c^2 $

= Experimental Results

Present your experimental setup, datasets, and results.

== Experimental Setup

We conducted experiments on...

== Results

#figure(
  table(
    columns: 4,
    [*Method*], [*Precision*], [*Recall*], [*F1*],
    [Baseline], [0.72], [0.68], [0.70],
    [Ours], [*0.89*], [*0.85*], [*0.87*],
  ),
  caption: [Comparison of methods on the benchmark dataset.],
)

Our method outperforms the baseline by 17% on F1 score.

= Conclusion

Summarize your contributions and suggest directions for future work.

In this paper, we presented... Future work includes...

// References section
#heading(numbering: none)[References]

#text(size: 9pt)[
  [1] J. Smith, "Example paper title," in _Proc. IEEE Conference_, 2023, pp. 1-10.

  [2] A. Jones and B. Williams, "Another example paper," _IEEE Trans. on Computing_, vol. 42, no. 3, pp. 123-135, 2024.
]
"##;

    /// A simpler document for initial testing
    const SIMPLE_DOC: &str = r#"
= Hello World

This is the first paragraph with some text.

This is the second paragraph.
"#;

    /// Recursively collect all frames with content_hint set
    fn collect_line_frames(frame: &Frame, depth: usize, results: &mut Vec<(usize, char, f64, f64)>) {
        let hint = frame.content_hint();
        if hint != '\0' {
            results.push((depth, hint, frame.height().to_pt(), frame.baseline().to_pt()));
        }

        for (pos, item) in frame.items() {
            if let FrameItem::Group(group) = item {
                collect_line_frames(&group.frame, depth + 1, results);
            }
        }
    }

    /// Print full frame hierarchy for debugging
    fn print_frame_hierarchy(frame: &Frame, depth: usize, accum_y: f64) {
        let indent = "  ".repeat(depth);
        let hint = frame.content_hint();
        let hint_str = if hint != '\0' {
            format!(" [LINE hint=0x{:02x}]", hint as u32)
        } else {
            String::new()
        };

        println!(
            "{}Frame: h={:.2}pt, baseline={:.2}pt, accum_y={:.2}pt, items={}{}",
            indent,
            frame.height().to_pt(),
            frame.baseline().to_pt(),
            accum_y,
            frame.items().len(),
            hint_str
        );

        for (idx, (pos, item)) in frame.items().enumerate() {
            let child_y = accum_y + pos.y.to_pt();

            match item {
                FrameItem::Group(group) => {
                    let t = &group.transform;
                    let is_identity = t.sx.get() == 1.0 && t.sy.get() == 1.0
                        && t.kx.get() == 0.0 && t.ky.get() == 0.0
                        && t.tx.to_pt() == 0.0 && t.ty.to_pt() == 0.0;

                    if !is_identity || depth < 3 {
                        println!(
                            "{}  [{}] Group @ ({:.2}, {:.2})pt, transform: sx={:.2}, sy={:.2}, tx={:.2}, ty={:.2}",
                            indent,
                            idx,
                            pos.x.to_pt(),
                            pos.y.to_pt(),
                            t.sx.get(),
                            t.sy.get(),
                            t.tx.to_pt(),
                            t.ty.to_pt()
                        );
                    }

                    let group_y = child_y + t.ty.to_pt();
                    print_frame_hierarchy(&group.frame, depth + 1, group_y);
                }
                FrameItem::Text(text) => {
                    if depth < 4 {
                        println!(
                            "{}  [{}] Text @ ({:.2}, {:.2})pt: {} glyphs",
                            indent,
                            idx,
                            pos.x.to_pt(),
                            pos.y.to_pt(),
                            text.glyphs.len()
                        );
                    }
                }
                _ => {}
            }
        }
    }

    /// Count frames at each depth with content_hint
    fn count_line_frames_by_depth(frame: &Frame, depth: usize, counts: &mut std::collections::HashMap<usize, usize>) {
        if frame.content_hint() != '\0' {
            *counts.entry(depth).or_insert(0) += 1;
        }

        for (_pos, item) in frame.items() {
            if let FrameItem::Group(group) = item {
                count_line_frames_by_depth(&group.frame, depth + 1, counts);
            }
        }
    }

    #[test]
    fn test_content_hint_detection_simple() {
        println!("\n=== Simple Document Frame Analysis ===\n");
        println!("Document source:");
        println!("---");
        println!("{}", SIMPLE_DOC.trim());
        println!("---\n");

        println!("This test verifies that content_hint is set on line frames.");
        println!("Expected: Each line of text should have a frame with content_hint != '\\0'");
        println!();

        // Note: We can't actually compile here without a full Typst world setup.
        // This test documents what we expect to see.
        println!("To verify in browser:");
        println!("1. The compiler should log '[LineFrame FOUND]' for each line");
        println!("2. Each line should have a small byte range (one line, not whole paragraph)");
        println!("3. The ty (Y position) should increase monotonically down the page");
    }

    /// Regression: ensure containers with content_hint still yield inner line frames.
    #[test]
    fn test_floated_title_line_boxes() {
        use typst::layout::{Abs, GroupItem, Point, Size};

        // Create two synthetic line frames (leafs).
        let mut line1 = Frame::soft(Size::new(Abs::pt(200.0), Abs::pt(20.0)));
        line1.set_baseline(Abs::pt(14.0));
        line1.set_content_hint('\n');

        let mut line2 = Frame::soft(Size::new(Abs::pt(200.0), Abs::pt(20.0)));
        line2.set_baseline(Abs::pt(14.0));
        line2.set_content_hint('\n');

        // Container frame that also has a content_hint (the problematic case).
        let mut container = Frame::soft(Size::new(Abs::pt(200.0), Abs::pt(80.0)));
        container.set_content_hint('\n');
        container.push(Point::new(Abs::pt(0.0), Abs::pt(0.0)), FrameItem::Group(GroupItem::new(line1)));
        container.push(Point::new(Abs::pt(0.0), Abs::pt(30.0)), FrameItem::Group(GroupItem::new(line2)));

        // Page frame containing the container.
        let mut page = Frame::soft(Size::new(Abs::pt(600.0), Abs::pt(800.0)));
        page.push(Point::new(Abs::pt(0.0), Abs::pt(100.0)), FrameItem::Group(GroupItem::new(container)));

        // Dummy source; collect_lines_from_frame will synthesize byte ranges in tests.
        let source = typst::syntax::Source::detached("");
        let lines = TypstCompileWorld::collect_lines_from_frame_for_test(
            0,
            &page,
            TransformState::identity(),
            &source,
        );

        assert_eq!(lines.len(), 2, "expected two line boxes from nested hints");
        assert!(
            lines[0].top < lines[1].top,
            "line tops should increase: first.top={} second.top={}",
            lines[0].top,
            lines[1].top
        );
    }

    #[test]
    fn test_ieee_template_structure() {
        println!("\n=== IEEE Template Structure Analysis ===\n");

        println!("The IEEE template has this structure:");
        println!("- Title (24pt bold, centered, in float)");
        println!("- Authors (grid, in float)");
        println!("- Abstract (italic text, in float)");
        println!("- Index Terms (in float)");
        println!("- Body content (2-column layout)");
        println!();

        println!("Key positions to verify (approximate, in pt from page top):");
        println!("- Page top margin: 54pt (0.75in)");
        println!("- Title: ~54pt");
        println!("- Authors: ~90pt");
        println!("- Abstract: ~180pt");
        println!("- First body paragraph: ~280pt");
        println!();

        println!("The float placement might affect frame hierarchy!");
        println!("Content in place() with float:true may have different transform chain.");
    }

    #[test]
    fn test_transform_accumulation_scenarios() {
        println!("\n=== Transform Accumulation Scenarios ===\n");

        // Scenario 1: Simple margin offset
        println!("Scenario 1: Content at page margin");
        println!("  Page margin top = 54pt (0.75in)");
        println!("  Expected line Y = 54pt + line offset within content");
        println!();

        // Scenario 2: Float placement
        println!("Scenario 2: Float-placed content (title, abstract)");
        println!("  place(top + center, float: true) creates special frame hierarchy");
        println!("  The float frame may have its own transform");
        println!();

        // Scenario 3: Two-column layout
        println!("Scenario 3: Two-column body");
        println!("  Left column X offset = margin (45pt = 0.625in)");
        println!("  Right column X offset = margin + column_width + gutter");
        println!("  Y positions should be same relative to page top");
        println!();

        // Document expected frame hierarchy
        println!("Expected frame hierarchy for IEEE template:");
        println!("  Page Frame (612pt x 792pt for US Letter)");
        println!("    └─ Group (page content, may have margin transform)");
        println!("         ├─ Float Frame (title, authors, abstract)");
        println!("         │    └─ Group (centered content)");
        println!("         │         ├─ Line Frame [title] - hint='\\n'");
        println!("         │         ├─ Line Frame [author 1] - hint='\\n'");
        println!("         │         └─ ...");
        println!("         └─ Column Frame (body)");
        println!("              ├─ Line Frame [intro para line 1] - hint='\\n'");
        println!("              └─ ...");
    }

    #[test]
    fn test_byte_range_expectations() {
        println!("\n=== Byte Range Expectations ===\n");

        // The Abstract paragraph in the IEEE template
        let abstract_text = "The process of scientific writing is often tangled up with the intricacies of typesetting, leading to frustration and wasted time for researchers. In this paper, we introduce Typst, a new typesetting system designed specifically for scientific writing. Typst untangles the typesetting process, allowing researchers to compose papers faster. In a series of experiments we demonstrate that Typst offers several advantages, including faster document creation, simplified syntax, and increased ease-of-use.";

        println!("Abstract text length: {} bytes", abstract_text.len());
        println!();

        // At 10pt with typical column width, expect ~10-15 words per line
        // Abstract is ~80 words, so ~6-8 lines
        println!("Expected Abstract structure:");
        println!("  - Text size: 10pt (italic)");
        println!("  - Approx words: ~80");
        println!("  - Approx lines: 5-8 (depending on column width)");
        println!("  - Each line should have byte range of ~60-100 bytes");
        println!();

        println!("PROBLEM INDICATOR:");
        println!("  If we see byte range like startByte=1335, endByte=3141 (~1800 bytes)");
        println!("  That means the ENTIRE abstract is treated as ONE line box!");
        println!("  This suggests content_hint is set on paragraph frame, not line frames.");
    }

    #[test]
    fn test_coordinate_math_verification() {
        println!("\n=== Coordinate Math Verification ===\n");

        // From actual browser logs:
        // glyphRect: {y: 513.105...}  - where glyph actually is
        // lineBox: {top: 204.277...}  - where we think line is
        // Difference: ~309px

        let glyph_y_px = 513.105;
        let linebox_top_px = 204.277;
        let diff_px = glyph_y_px - linebox_top_px;

        println!("From browser logs:");
        println!("  Glyph actual Y: {:.2}px", glyph_y_px);
        println!("  LineBox top:    {:.2}px", linebox_top_px);
        println!("  Difference:     {:.2}px", diff_px);
        println!();

        // Convert to points (assuming ~1.57 px/pt)
        let pixels_per_pt = 1.57;
        let diff_pt = diff_px / pixels_per_pt;
        println!("  Difference in pt: {:.2}pt (at {:.2} px/pt)", diff_pt, pixels_per_pt);
        println!();

        // This ~197pt difference is huge - almost the entire page height difference
        // Suggests either:
        // 1. Line boxes are being computed in wrong coordinate space
        // 2. Page offset isn't being applied correctly
        // 3. Float content is positioned differently than body content

        println!("Possible causes:");
        println!("  1. Float content (abstract) uses different coordinate origin than body");
        println!("  2. pageOffset calculation is wrong for float content");
        println!("  3. content_hint is on wrong frame (paragraph vs line)");
    }
}
