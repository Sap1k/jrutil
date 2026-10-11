// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil

/// The optional route-shapes package phase (Serving/RouteShapes.fs; Oběhy
/// BASE_PLAN.md section 17): trips still without a shape are map-matched by
/// the route-shapes companion (`obehy-motis-shapes`) on a prepared osr graph.
type RouteShapeOptions = {
    /// The companion executable.
    companion: string
    /// Graph directory written by `obehy-motis-shapes extract`.
    graph: string
    /// Companion result cache reused across builds.
    cache: string option
}
