import XCTest
@testable import PygmentsSwift

final class CapnProtoLexerTests: XCTestCase {
    func testCapnProtoBasics() {
        let lexer = CapnProtoLexer()
        let input = """
        # comment
        @0xdeadbeefdeadbeef;
        struct Item { n @0 :Int32; label @1 :Text; }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "struct" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
