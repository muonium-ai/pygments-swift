import XCTest
@testable import PygmentsSwift

final class DiffLexerTests: XCTestCase {
    func testDiffBasics() {
        let lexer = DiffLexer()
        let input = """
        diff --git a/a.txt b/a.txt
        --- a/a.txt
        +++ b/a.txt
        @@ -1,1 +1,1 @@
        -old
        +new
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(250).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type == .generic.child("Inserted") }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .generic.child("Deleted") }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .generic.child("Subheading") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
