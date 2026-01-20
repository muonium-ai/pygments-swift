import Foundation

/// Minimal Dockerfile lexer.
public final class DockerfileLexer: RegexLexer {
    public override var tokens: [String: [Rule]] {
        [
            "root": [
                Rule("[\\t ]+", action: .token(.whitespace)),
                Rule("\\n", action: .token(.whitespace)),

                Rule("#[^\\n]*", action: .token(.comment.child("Single"))),

                Rule(
                    "^(FROM|RUN|CMD|LABEL|MAINTAINER|EXPOSE|ENV|ADD|COPY|ENTRYPOINT|VOLUME|USER|WORKDIR|ARG|ONBUILD|STOPSIGNAL|HEALTHCHECK|SHELL)\\b",
                    options: [.anchorsMatchLines, .caseInsensitive],
                    action: .token(.keyword)
                ),

                // Key=Value (common in ARG/ENV)
                Rule(
                    "([A-Za-z_][A-Za-z0-9_]*)(=)",
                    action: .byGroups([.name.child("Attribute"), .operator])
                ),

                Rule("\\$\\{?[A-Za-z_][A-Za-z0-9_]*\\}?", action: .token(.name.child("Variable"))),
                Rule("\"(?:[^\"\\\\]|\\\\.)*\"", action: .token(.string)),
                Rule("'[^']*'", action: .token(.string)),

                Rule("[^\\n#]+", action: .token(.text)),
            ]
        ]
    }
}
