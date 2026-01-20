import XCTest
@testable import PygmentsSwift

final class SvelteLexerTests: XCTestCase {
    func testSvelteBasics() {
        let lexer = SvelteLexer()
        let input = #"""
        <script>
          let count = 0
        </script>

        <button on:click={() => count++}>{count}</button>
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .name) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .punctuation) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
