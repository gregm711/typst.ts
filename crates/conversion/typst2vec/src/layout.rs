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
