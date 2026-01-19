import XCTest
@testable import PygmentsSwift

final class PHPLexerTests: XCTestCase {
    func testPHPLexingBasics() {
        let lexer = PHPLexer()
        let input = """
        <?php
        namespace App\\Demo;

        // comment
        class Greeter {
            public function greet(int $x): string {
                $y = \"hi $x\";
                /* block comment */
                return $y;
            }
        }

        $v = match ($x) {
            1 => 'one',
            default => `echo ok`,
        };
        ?>
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(250).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "function" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .name.child("Variable") && $0.value == "$x" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .number) }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
