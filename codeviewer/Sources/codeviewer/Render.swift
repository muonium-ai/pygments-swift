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
    private struct TextStats {
        let lineCount: Int
        let maxLineCharacters: Int

        static func from(_ attributed: NSAttributedString) -> TextStats {
            let s = attributed.string
            if s.isEmpty {
                return TextStats(lineCount: 1, maxLineCharacters: 0)
            }
            var lines = s.split(separator: "\n", omittingEmptySubsequences: false)
            if lines.isEmpty { lines = [""] }
            var maxChars = 0
            for line in lines {
                maxChars = max(maxChars, line.count)
            }
            return TextStats(lineCount: max(1, lines.count), maxLineCharacters: maxChars)
        }
    }

    private static func estimateWidth(attributed: NSAttributedString, padding: CGFloat) -> CGFloat {
        let stats = TextStats.from(attributed)

        // Prefer the actual font if present; otherwise fall back to a monospaced guess.
        let font = (attributed.attribute(.font, at: 0, effectiveRange: nil) as? NSFont)
            ?? NSFont.monospacedSystemFont(ofSize: 13, weight: .regular)

        // For monospaced fonts, using a single-glyph width is a decent estimate.
        let glyphWidth = ("M" as NSString).size(withAttributes: [.font: font]).width
        // Add a small safety margin so we don't accidentally wrap due to rounding.
        let contentWidth = ceil(CGFloat(stats.maxLineCharacters) * glyphWidth + glyphWidth * 2)
        return max(1, contentWidth + padding * 2)
    }

    static func makeTextView(attributed: NSAttributedString, options: RenderOptions) -> NSTextView {
        let initialWidth: CGFloat = {
            if let w = options.width { return w }
            return estimateWidth(attributed: attributed, padding: options.padding)
        }()

        let containerSize = NSSize(
            width: max(1, initialWidth - options.padding * 2),
            height: CGFloat.greatestFiniteMagnitude
        )

        let textStorage = NSTextStorage(attributedString: attributed)
        let layoutManager = NSLayoutManager()
        let textContainer = NSTextContainer(containerSize: containerSize)
        textContainer.lineFragmentPadding = 0
        textContainer.lineBreakMode = .byCharWrapping
        // Important for offscreen layout: don't let the container height track the view.
        // Otherwise the initial 1pt-high frame can limit layout and produce cropped PNGs.
        textContainer.heightTracksTextView = false
        layoutManager.addTextContainer(textContainer)
        textStorage.addLayoutManager(layoutManager)

        let textView = NSTextView(frame: .zero, textContainer: textContainer)
        textView.drawsBackground = true
        textView.backgroundColor = options.background
        textView.isEditable = false
        textView.isSelectable = false
        textView.textContainerInset = NSSize(width: options.padding, height: options.padding)
        textView.isVerticallyResizable = true
        textView.isHorizontallyResizable = false
        textView.minSize = NSSize(width: 0, height: 0)
        textView.maxSize = NSSize(width: initialWidth, height: CGFloat.greatestFiniteMagnitude)

        // IMPORTANT: keep the container width explicit. Tracking the textView width can be
        // problematic before the view has its final frame, and it can lead to incorrect
        // layout caching (PDF reflows later; PNG often doesn't).
        textView.textContainer?.widthTracksTextView = false
        textView.textContainer?.heightTracksTextView = false
        textView.textContainer?.containerSize = containerSize

        // Give the view a width up-front so layout happens against the right constraint.
        textView.frame = NSRect(x: 0, y: 0, width: initialWidth, height: 1)

        // First layout pass (measuring).
        layoutManager.ensureLayout(for: textContainer)
        var used = layoutManager.usedRect(for: textContainer)

        // Final width:
        // - If the user requested a width, honor it.
        // - Otherwise, use our estimate but also expand if the laid-out text is wider.
        let finalWidth: CGFloat = {
            if let w = options.width { return w }
            return max(initialWidth, ceil(used.width + options.padding * 2))
        }()

        // Update container width if we changed finalWidth.
        textView.textContainer?.containerSize = NSSize(
            width: max(1, finalWidth - options.padding * 2),
            height: CGFloat.greatestFiniteMagnitude
        )

        // Second layout pass (finalizing) so PNG drawing uses the correct cached layout.
        layoutManager.invalidateLayout(forCharacterRange: NSRange(location: 0, length: textStorage.length), actualCharacterRange: nil)
        layoutManager.ensureLayout(for: textContainer)
        used = layoutManager.usedRect(for: textContainer)

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
        view.wantsLayer = true

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

        guard let ctx = NSGraphicsContext(bitmapImageRep: rep) else {
            throw CLIError("Failed to create graphics context")
        }
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = ctx

        // High quality text rendering.
        ctx.imageInterpolation = .high
        ctx.shouldAntialias = true

        // Apply scaling.
        ctx.cgContext.scaleBy(x: scale, y: scale)
        view.layer?.contentsScale = scale

        view.displayIgnoringOpacity(bounds, in: ctx)

        NSGraphicsContext.restoreGraphicsState()

        guard let data = rep.representation(using: .png, properties: [:]) else {
            throw CLIError("Failed to encode PNG")
        }
        return data
    }
}
