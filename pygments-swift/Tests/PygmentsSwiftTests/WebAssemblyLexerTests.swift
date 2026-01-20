import XCTest
@testable import PygmentsSwift

final class WebAssemblyLexerTests: XCTestCase {
    func testWebAssemblyBasics() {
        let lexer = WebAssemblyLexer()
        let input = """
        ;; comment
        (module
          (func $fib (param $n i32) (result i32)
            local.get $n
            i32.const 2
            i32.lt_s)
          (export "fib" (func $fib)))
        """

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(350).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .keyword && $0.value == "module" }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .string) && $0.value.contains("fib") }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
