import Foundation
import PygmentsSwift

enum HTMLRender {
    static func renderHTML(tokens: [Token], theme: HighlightTheme, title: String) -> String {
        let bg = cssHex(theme.background)
        let fg = cssHex(theme.defaultForeground)

        var out = ""
        out += "<!doctype html>\n"
        out += "<html lang=\"en\">\n"
        out += "<head>\n"
        out += "  <meta charset=\"utf-8\">\n"
        out += "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
        out += "  <title>\(escapeHTML(title))</title>\n"
        out += "  <style>\n"
        out += "    :root { --bg: \(bg); --fg: \(fg); }\n"
        out += "    html, body { height: 100%; }\n"
        out += "    body { margin: 0; background: var(--bg); color: var(--fg); }\n"
        out += "    pre {\n"
        out += "      margin: 0;\n"
        out += "      padding: 18px;\n"
        out += "      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, \"Liberation Mono\", \"Courier New\", monospace;\n"
        out += "      font-size: 13px;\n"
        out += "      line-height: 1.35;\n"
        out += "      white-space: pre;\n"
        out += "      tab-size: 4;\n"
        out += "    }\n"
        out += "    .tok { white-space: pre; }\n"
        out += "  </style>\n"
        out += "</head>\n"
        out += "<body>\n"
        out += "<pre>"

        for t in tokens {
            let color = cssHex(theme.foreground(for: t.type))
            let tokenName = escapeHTML(t.type.description)
            out += "<span class=\"tok\" data-token=\"\(tokenName)\" style=\"color: \(color)\">\(escapeHTML(t.value))</span>"
        }

        out += "</pre>\n"
        out += "</body>\n"
        out += "</html>\n"
        return out
    }

    private static func escapeHTML(_ s: String) -> String {
        var out = ""
        out.reserveCapacity(s.count)
        for ch in s {
            switch ch {
            case "&": out += "&amp;"
            case "<": out += "&lt;"
            case ">": out += "&gt;"
            case "\"": out += "&quot;"
            default:
                out.append(ch)
            }
        }
        return out
    }

    private static func cssHex(_ c: RGBAColor) -> String {
        func clamp01(_ x: Double) -> Double { min(1.0, max(0.0, x)) }
        let r = Int((clamp01(c.r) * 255.0).rounded())
        let g = Int((clamp01(c.g) * 255.0).rounded())
        let b = Int((clamp01(c.b) * 255.0).rounded())
        let a = clamp01(c.a)
        if a >= 0.999 {
            return String(format: "#%02X%02X%02X", r, g, b)
        }
        return String(format: "rgba(%d,%d,%d,%.3f)", r, g, b, a)
    }
}
