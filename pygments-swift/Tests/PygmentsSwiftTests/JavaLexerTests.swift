import XCTest
@testable import PygmentsSwift

final class JavaLexerTests: XCTestCase {
    func testJavaLexingBasics() {
        let lexer = JavaLexer()
        let input = """
        package demo;

        // comment
        @Deprecated
        public class Foo {
            public static void main(String[] args) {
                String s = "hi\\n";
                int x = 42;
            }
        }
        """

        let tokens = lexer.getTokens(input)

        let summary = tokens.prefix(120).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Decorator") && $0.value == "@Deprecated" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Class") && $0.value == "Foo" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
