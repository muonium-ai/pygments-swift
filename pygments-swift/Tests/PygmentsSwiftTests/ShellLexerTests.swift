import XCTest
@testable import PygmentsSwift

final class ShellLexerTests: XCTestCase {
    func testShellLexingBasics() {
        let lexer = ShellLexer()
        let input = """
        #!/usr/bin/env bash
        set -euo pipefail

        # comment
        name="world"
        echo "hi $name" | sed 's/world/WORLD/'

        if [ "$name" != "" ]; then
          echo ${name}
        fi

        out=$(printf "%s" "$name")
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(250).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "if" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Variable") && $0.value.contains("name") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
