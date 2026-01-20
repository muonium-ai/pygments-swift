import XCTest
@testable import PygmentsSwift

final class CudaLexerTests: XCTestCase {
    func testCudaBasics() {
        let lexer = CudaLexer()
        let input = #"""
        __global__ void kernel() {
          int i = threadIdx.x;
        }
        kernel<<<1, 1>>>();
        /* comment */
        """#

        let tokens = lexer.getTokens(input)
        let summary = tokens.prefix(400).map { "\($0.type)=\($0.value.debugDescription)" }.joined(separator: ", ")

        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .keyword) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type.isSubtype(of: .comment) }), summary)
        XCTAssertTrue(tokens.contains(where: { $0.type == .operator && $0.value == "<<<" }), summary)
        XCTAssertFalse(tokens.contains(where: { $0.type == .error }), summary)
    }
}
