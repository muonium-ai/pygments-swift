public struct RGBAColor: Hashable, Sendable {
    public var r: Double
    public var g: Double
    public var b: Double
    public var a: Double

    public init(r: Double, g: Double, b: Double, a: Double = 1.0) {
        self.r = r
        self.g = g
        self.b = b
        self.a = a
    }

    public init(_ r255: Int, _ g255: Int, _ b255: Int, alpha a: Double = 1.0) {
        self.r = Double(r255) / 255.0
        self.g = Double(g255) / 255.0
        self.b = Double(b255) / 255.0
        self.a = a
    }

    public init(hex rgb: UInt32, alpha a: Double = 1.0) {
        let r255 = Int((rgb >> 16) & 0xFF)
        let g255 = Int((rgb >> 8) & 0xFF)
        let b255 = Int(rgb & 0xFF)
        self.init(r255, g255, b255, alpha: a)
    }
}
