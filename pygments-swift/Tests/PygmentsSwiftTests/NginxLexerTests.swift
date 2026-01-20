import XCTest
@testable import PygmentsSwift

final class NginxLexerTests: XCTestCase {
    func testNginxBasics() {
        let lexer = NginxLexer()
        let input = """
        # comment
        server { listen 8080; set $n 10; return 200 "n=$n"; }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(300).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Variable") && $0.value == "$n" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Attribute") && $0.value == "server" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
