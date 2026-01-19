import Foundation

/// JSON lexer matching Pygments' `JsonLexer` (`pygments.lexers.data.JsonLexer`).
///
/// Notes:
/// - Supports JavaScript-style comments (`//` and `/* ... */`) like Pygments.
/// - No validation is performed; tokenization is character-set based.
public final class JsonLexer: LexerBase {
    public override func getTokens(_ text: String) -> [Token] {
        let processed = preprocess(text)
        return getTokensUnprocessed(processed)
    }

    private func getTokensUnprocessed(_ text: String) -> [Token] {
        var inString = false
        var inEscape = false
        var inUnicodeEscape = 0
        var inWhitespace = false
        var inConstant = false
        var inNumber = false
        var inFloat = false
        var inPunctuation = false
        var inCommentSingle = false
        var inCommentMultiline = false
        var expectingSecondCommentOpener = false // // or /*
        var expectingSecondCommentCloser = false // */

        // Character sets mirror the Python lexer.
        let integerChars = Set("-0123456789")
        let floatChars = Set(".eE+")
        let constantChars = Set("truefalsenull")
        let hexChars = Set("0123456789abcdefABCDEF")
        let punctuationChars = Set("{}[],")
        let whitespaceScalars: Set<UnicodeScalar> = [" ", "\n", "\r", "\t"]

        var startUTF16 = 0
        var startScalar = 0

        let scalars = text.unicodeScalars
        var startIndex = scalars.startIndex

        // Queue used to re-tokenize quoted strings as Name.Tag when followed by ':'
        // (object keys vs string values), matching Pygments.
        struct Queued {
            let startUTF16: Int
            let startScalar: Int
            let type: TokenType
            let value: String
        }
        var queue: [Queued] = []

        var out: [Token] = []

        func emit(_ startUTF16: Int, _ startScalar: Int, _ type: TokenType, _ value: String) {
            out.append(Token(start: startUTF16, startScalar: startScalar, type: type, value: value))
        }

        func flushQueue(verbatim: Bool = true) {
            if verbatim {
                for q in queue {
                    emit(q.startUTF16, q.startScalar, q.type, q.value)
                }
            }
            queue.removeAll(keepingCapacity: true)
        }

        var utf16Offset = 0
        var scalarOffset = 0
        var i = scalars.startIndex

        while i < scalars.endIndex {
            let ch = scalars[i]
            let next = scalars.index(after: i)
            let nextUTF16Offset = utf16Offset + ch.utf16.count
            let nextScalarOffset = scalarOffset + 1

            var reprocess = true
            while reprocess {
                reprocess = false

                if inString {
                    if inUnicodeEscape > 0 {
                        if hexChars.contains(Character(ch)) {
                            inUnicodeEscape -= 1
                            if inUnicodeEscape == 0 {
                                inEscape = false
                            }
                        } else {
                            inUnicodeEscape = 0
                            inEscape = false
                        }
                    } else if inEscape {
                        if ch == "u" {
                            inUnicodeEscape = 4
                        } else {
                            inEscape = false
                        }
                    } else if ch == "\\" {
                        inEscape = true
                    } else if ch == "\"" {
                        let value = String(scalars[startIndex..<next])
                        queue.append(Queued(startUTF16: startUTF16, startScalar: startScalar, type: .string.child("Double"), value: value))
                        inString = false
                        inEscape = false
                        inUnicodeEscape = 0
                    }

                    // Always consume characters while inside a string.
                    i = next
                    utf16Offset = nextUTF16Offset
                    scalarOffset = nextScalarOffset
                    continue
                }

                if inWhitespace {
                    if whitespaceScalars.contains(ch) {
                        // Keep consuming whitespace.
                        i = next
                        utf16Offset = nextUTF16Offset
                        scalarOffset = nextScalarOffset
                        continue
                    }

                    let value = String(scalars[startIndex..<i])
                    if !value.isEmpty {
                        if !queue.isEmpty {
                            queue.append(Queued(startUTF16: startUTF16, startScalar: startScalar, type: .whitespace, value: value))
                        } else {
                            emit(startUTF16, startScalar, .whitespace, value)
                        }
                    }
                    inWhitespace = false
                    reprocess = true
                    continue
                }

                if inConstant {
                    if constantChars.contains(Character(ch)) {
                        i = next
                        utf16Offset = nextUTF16Offset
                        scalarOffset = nextScalarOffset
                        continue
                    }

                    let value = String(scalars[startIndex..<i])
                    if !value.isEmpty {
                        emit(startUTF16, startScalar, .keyword.child("Constant"), value)
                    }
                    inConstant = false
                    reprocess = true
                    continue
                }

                if inNumber {
                    if integerChars.contains(Character(ch)) {
                        i = next
                        utf16Offset = nextUTF16Offset
                        scalarOffset = nextScalarOffset
                        continue
                    } else if floatChars.contains(Character(ch)) {
                        inFloat = true
                        i = next
                        utf16Offset = nextUTF16Offset
                        scalarOffset = nextScalarOffset
                        continue
                    }

                    let value = String(scalars[startIndex..<i])
                    if !value.isEmpty {
                        emit(startUTF16, startScalar, inFloat ? .number.child("Float") : .number.child("Integer"), value)
                    }
                    inNumber = false
                    inFloat = false
                    reprocess = true
                    continue
                }

                if inPunctuation {
                    if punctuationChars.contains(Character(ch)) {
                        i = next
                        utf16Offset = nextUTF16Offset
                        scalarOffset = nextScalarOffset
                        continue
                    }

                    let value = String(scalars[startIndex..<i])
                    if !value.isEmpty {
                        emit(startUTF16, startScalar, .punctuation, value)
                    }
                    inPunctuation = false
                    reprocess = true
                    continue
                }

                if inCommentSingle {
                    if ch != "\n" {
                        i = next
                        utf16Offset = nextUTF16Offset
                        scalarOffset = nextScalarOffset
                        continue
                    }

                    let value = String(scalars[startIndex..<i])
                    if !value.isEmpty {
                        if !queue.isEmpty {
                            queue.append(Queued(startUTF16: startUTF16, startScalar: startScalar, type: .comment.child("Single"), value: value))
                        } else {
                            emit(startUTF16, startScalar, .comment.child("Single"), value)
                        }
                    }
                    inCommentSingle = false
                    reprocess = true
                    continue
                }

                if inCommentMultiline {
                    if ch == "*" {
                        expectingSecondCommentCloser = true
                    } else if expectingSecondCommentCloser {
                        expectingSecondCommentCloser = false
                        if ch == "/" {
                            let value = String(scalars[startIndex..<next])
                            if !value.isEmpty {
                                if !queue.isEmpty {
                                    queue.append(Queued(startUTF16: startUTF16, startScalar: startScalar, type: .comment.child("Multiline"), value: value))
                                } else {
                                    emit(startUTF16, startScalar, .comment.child("Multiline"), value)
                                }
                            }
                            inCommentMultiline = false
                        }
                    }

                    i = next
                    utf16Offset = nextUTF16Offset
                    scalarOffset = nextScalarOffset
                    continue
                }

                if expectingSecondCommentOpener {
                    expectingSecondCommentOpener = false
                    if ch == "/" {
                        inCommentSingle = true
                        i = next
                        utf16Offset = nextUTF16Offset
                        scalarOffset = nextScalarOffset
                        continue
                    } else if ch == "*" {
                        inCommentMultiline = true
                        i = next
                        utf16Offset = nextUTF16Offset
                        scalarOffset = nextScalarOffset
                        continue
                    }

                    // Exhaust the queue. Accept the existing token types.
                    flushQueue()

                    // Emit error token for the first '/' (from startIndex..<i).
                    let value = String(scalars[startIndex..<i])
                    if !value.isEmpty {
                        emit(startUTF16, startScalar, .error, value)
                    }

                    reprocess = true
                    continue
                }

                // Begin evaluating a new token at the current position.
                startUTF16 = utf16Offset
                startScalar = scalarOffset
                startIndex = i

                if ch == "\"" {
                    inString = true
                } else if whitespaceScalars.contains(ch) {
                    inWhitespace = true
                } else if ch == "f" || ch == "n" || ch == "t" {
                    flushQueue()
                    inConstant = true
                } else if integerChars.contains(Character(ch)) {
                    flushQueue()
                    inNumber = true
                } else if ch == ":" {
                    // Yield from the queue; replace quoted strings with Name.Tag.
                    for q in queue {
                        if q.type == .string.child("Double") {
                            emit(q.startUTF16, q.startScalar, .name.child("Tag"), q.value)
                        } else {
                            emit(q.startUTF16, q.startScalar, q.type, q.value)
                        }
                    }
                    queue.removeAll(keepingCapacity: true)
                    inPunctuation = true
                } else if punctuationChars.contains(Character(ch)) {
                    flushQueue()
                    inPunctuation = true
                } else if ch == "/" {
                    expectingSecondCommentOpener = true
                } else {
                    flushQueue()
                    emit(startUTF16, startScalar, .error, String(ch))
                }

                // Consume the current character in the outer loop.
                i = next
                utf16Offset = nextUTF16Offset
                scalarOffset = nextScalarOffset
            }
        }

        // Yield any remaining queued tokens.
        flushQueue()

        // Yield remaining unfinished token.
        if inString {
            emit(startUTF16, startScalar, .error, String(scalars[startIndex..<scalars.endIndex]))
        } else if inFloat {
            emit(startUTF16, startScalar, .number.child("Float"), String(scalars[startIndex..<scalars.endIndex]))
        } else if inNumber {
            emit(startUTF16, startScalar, .number.child("Integer"), String(scalars[startIndex..<scalars.endIndex]))
        } else if inConstant {
            emit(startUTF16, startScalar, .keyword.child("Constant"), String(scalars[startIndex..<scalars.endIndex]))
        } else if inWhitespace {
            emit(startUTF16, startScalar, .whitespace, String(scalars[startIndex..<scalars.endIndex]))
        } else if inPunctuation {
            emit(startUTF16, startScalar, .punctuation, String(scalars[startIndex..<scalars.endIndex]))
        } else if inCommentSingle {
            emit(startUTF16, startScalar, .comment.child("Single"), String(scalars[startIndex..<scalars.endIndex]))
        } else if inCommentMultiline {
            emit(startUTF16, startScalar, .error, String(scalars[startIndex..<scalars.endIndex]))
        } else if expectingSecondCommentOpener {
            emit(startUTF16, startScalar, .error, String(scalars[startIndex..<scalars.endIndex]))
        }

        return out
    }
}
