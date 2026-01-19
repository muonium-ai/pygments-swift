import AppKit
import Foundation

struct RenderOptions {
    var width: CGFloat?
    var padding: CGFloat
    var background: NSColor
    var foreground: NSColor

    static var `default`: RenderOptions {
        RenderOptions(width: 900, padding: 18, background: NSColor(calibratedWhite: 1, alpha: 1), foreground: NSColor(calibratedWhite: 0.1, alpha: 1))
    }
}

enum CodeRender {
    static func makeTextView(attributed: NSAttributedString, options: RenderOptions) -> NSTextView {
        let containerSize = NSSize(width: options.width.map { max(1, $0 - options.padding * 2) } ?? .greatestFiniteMagnitude,
                                  height: .greatestFiniteMagnitude)

        let textStorage = NSTextStorage(attributedString: attributed)
        let layoutManager = NSLayoutManager()
        let textContainer = NSTextContainer(containerSize: containerSize)
        textContainer.lineFragmentPadding = 0
        textContainer.lineBreakMode = .byCharWrapping
        layoutManager.addTextContainer(textContainer)
        textStorage.addLayoutManager(layoutManager)

        let textView = NSTextView(frame: .zero, textContainer: textContainer)
        textView.drawsBackground = true
        textView.backgroundColor = options.background
        textView.isEditable = false
        textView.isSelectable = false
        textView.textContainerInset = NSSize(width: options.padding, height: options.padding)
        // Base color (attributed runs still override this).
        textView.textColor = options.foreground
        textView.isVerticallyResizable = true
        textView.isHorizontallyResizable = false

        // Wrap to width (default). If width is nil, allow the layout to be as wide as it needs.
        if options.width == nil {
            textView.isHorizontallyResizable = true
            textView.textContainer?.widthTracksTextView = false
            textView.textContainer?.containerSize = NSSize(width: CGFloat.greatestFiniteMagnitude, height: CGFloat.greatestFiniteMagnitude)
        } else {
            textView.textContainer?.widthTracksTextView = true
        }

        // Force layout so we can measure.
        layoutManager.ensureLayout(for: textContainer)
        let used = layoutManager.usedRect(for: textContainer)

        let finalWidth: CGFloat
        if let w = options.width {
            finalWidth = w
        } else {
            finalWidth = ceil(used.width + options.padding * 2)
        }

        let finalHeight = ceil(used.height + options.padding * 2)
        textView.frame = NSRect(x: 0, y: 0, width: finalWidth, height: finalHeight)
        return textView
    }

    static func renderPDF(attributed: NSAttributedString, options: RenderOptions) -> Data {
        let view = makeTextView(attributed: attributed, options: options)
        return view.dataWithPDF(inside: view.bounds)
    }

    static func renderPNG(attributed: NSAttributedString, options: RenderOptions, scale: CGFloat = 2.0) throws -> Data {
        let view = makeTextView(attributed: attributed, options: options)

        let bounds = view.bounds
        let pixelSize = NSSize(width: bounds.width * scale, height: bounds.height * scale)

        guard let rep = NSBitmapImageRep(
            bitmapDataPlanes: nil,
            pixelsWide: Int(pixelSize.width.rounded(.up)),
            pixelsHigh: Int(pixelSize.height.rounded(.up)),
            bitsPerSample: 8,
            samplesPerPixel: 4,
            hasAlpha: true,
            isPlanar: false,
            colorSpaceName: .deviceRGB,
            bytesPerRow: 0,
            bitsPerPixel: 0
        ) else {
            throw CLIError("Failed to allocate bitmap")
        }

        rep.size = NSSize(width: bounds.width, height: bounds.height)

        let ctx = NSGraphicsContext(bitmapImageRep: rep)
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = ctx

        // High quality text rendering.
        ctx?.imageInterpolation = .high
        ctx?.shouldAntialias = true

        // Apply scaling.
        NSGraphicsContext.current?.cgContext.scaleBy(x: scale, y: scale)
        view.layer?.contentsScale = scale

        view.displayIgnoringOpacity(bounds, in: ctx!)

        NSGraphicsContext.restoreGraphicsState()

        guard let data = rep.representation(using: .png, properties: [:]) else {
            throw CLIError("Failed to encode PNG")
        }
        return data
    }
}
