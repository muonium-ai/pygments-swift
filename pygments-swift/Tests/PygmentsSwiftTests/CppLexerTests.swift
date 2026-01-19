import XCTest
@testable import PygmentsSwift

final class CppLexerTests: XCTestCase {
    func testCppLexingBasics() {
        let lexer = CppLexer()
        let input = """
        #include <vector>
        // comment
        namespace foo { struct Bar { int x = 1; }; }
        class Baz final { public: bool ok() const noexcept { return true; } };
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(160).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment.child("Preproc")) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Namespace") && $0.value == "foo" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Class") && $0.value == "Baz" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword.child("Type") && $0.value == "bool" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword.child("Constant") && $0.value == "true" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
