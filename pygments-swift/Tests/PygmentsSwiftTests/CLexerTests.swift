import XCTest
@testable import PygmentsSwift

final class CLexerTests: XCTestCase {
    func testCLexingBasics() {
        let lexer = CLexer()
        let input = """
        #include <stdio.h>
        // comment
        /* block */
        static inline int add(int a, int b) { return a + b; }
        int main(void) { printf("hi\\n"); return 0; }
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(120).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment.child("Preproc")) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "return" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword.child("Type") && $0.value == "int" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
