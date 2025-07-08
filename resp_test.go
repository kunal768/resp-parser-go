package main // Or your actual package name, e.g., 'package resp_parser'

import (
	"fmt"
	"reflect" // For DeepEqual comparison of DataType structs
	"testing" // Import the testing package
	// Assuming resp.go is in the same package, no explicit import needed if it's 'package main'.
	// If it's a different package (e.g., 'package resp_parser' for resp.go and this test),
	// you'd import it like: "your_module/resp_parser"
)

// TestRespSimpleStringParser tests RESP Simple String parsing.
func TestRespSimpleStringParser(t *testing.T) {
	svc := NewRespService() // Initialize your service

	tests := []struct {
		name    string
		input   string
		wantID  byte
		wantVal any // Expected value for DataType.ReturnType
		wantErr bool
	}{
		// --- Valid Simple Strings ---
		{"Basic OK", "+OK\r\n", '+', "OK", false},
		{"Basic PONG", "+PONG\r\n", '+', "PONG", false},
		{"Empty string", "+\r\n", '+', "", false},
		{"Whitespace string", "+   \r\n", '+', "   ", false},
		{"Alphanumeric", "+foo123\r\n", '+', "foo123", false},
		{"Special ASCII", "+!@#$%^&*\r\n", '+', "!@#$%^&*", false},
		{"Spaces", "+hello world\r\n", '+', "hello world", false},
		{"Tabs", "+\thello\t\r\n", '+', "\thello\t", false},
		{"Numbers as string", "+12345\r\n", '+', "12345", false},

		// --- Invalid Simple Strings (Malformed) ---
		{"No CRLF", "+OK", '+', nil, true},                      // Missing \r\n
		{"Only CR", "+OK\r", '+', nil, true},                    // Missing \n
		{"Only LF", "+OK\n", '+', nil, true},                    // Missing \r (or incorrect order)
		{"No plus prefix", "OK\r\n", 'O', nil, true},            // Missing '+' prefix, first byte 'O'
		{"Contains CR in content", "+O\rK\r\n", '+', nil, true}, // Content contains CR, illegal
		{"Contains LF in content", "+O\nK\r\n", '+', nil, true}, // Content contains LF, illegal
		{"Empty input", "", 0, nil, true},                       // No data to parse
		{"Incomplete simple string", "+FOO", '+', nil, true},    // Missing \r\n
		{"Incomplete plus (only +)", "+", '+', nil, true},       // Only prefix, no content/terminator

		// --- Trailing Data (Errors if Parse is for a single RESP element) ---
		{"Trailing data", "+foo\r\n+bar\r\n", '+', "foo", true},           // Expect error due to trailing data
		{"Trailing data, invalid", "+foo\r\n+bar\n", '+', "foo", true},    // Parses "foo" but errors on invalid trailing data
		{"Trailing data, no prefix", "+foo\r\nbar\r\n", '+', "foo", true}, // Parses "foo" but errors on trailing data
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var dataType DataType
			var err error
			var panicked bool

			func() {
				defer func() {
					if r := recover(); r != nil {
						panicked = true
						if e, ok := r.(error); ok {
							err = e
						} else {
							err = fmt.Errorf("panic: %v", r)
						}
					}
				}()
				dataType, err = svc.Parse([]byte(tc.input))
			}()

			if tc.wantErr {
				if err == nil && !panicked {
					t.Errorf("Expected an error for input %q, but got none. Result: %+v", tc.input, dataType)
				}
			} else {
				if err != nil {
					t.Errorf("Did not expect an error for input %q, but got: %v", tc.input, err)
				}
				if panicked {
					t.Errorf("Unexpected panic for input %q: %v", tc.input, err)
				}
				if dataType.ID != tc.wantID {
					t.Errorf("ID mismatch for input %q: want %q, got %q", tc.input, rune(tc.wantID), rune(dataType.ID))
				}
				if !reflect.DeepEqual(dataType.ReturnType, tc.wantVal) {
					t.Errorf("Value mismatch for input %q: want %v (%T), got %v (%T)", tc.input, tc.wantVal, tc.wantVal, dataType.ReturnType, dataType.ReturnType)
				}
			}
			// Log success if all checks pass for a non-error case, or if an error was expected and received.
			if t.Failed() {
				t.Logf("Failed: %s", tc.name)
				if err != nil {
					t.Logf("Actual Error: %v", err)
				}
			} else {
				t.Logf("Passed: %s", tc.name)
			}
		})
	}
}

// TestRespSimpleErrorParser tests RESP Simple Error parsing.
func TestRespSimpleErrorParser(t *testing.T) {
	svc := NewRespService() // Initialize your service

	tests := []struct {
		name    string
		input   string
		wantID  byte
		wantVal any // Expected error message content
		wantErr bool
	}{
		// --- Valid Simple Errors ---
		{"Basic Error", "-ERR unknown command 'foobar'\r\n", '-', "ERR unknown command 'foobar'", false},
		{"Type Error", "-WRONGTYPE Operation against a key holding the wrong kind of value\r\n", '-', "WRONGTYPE Operation against a key holding the wrong kind of value", false},
		{"Empty Error", "-\r\n", '-', "", false}, // Valid error with an empty message
		{"Error with Special Chars", "-Error!@#$%^&*()\r\n", '-', "Error!@#$%^&*()", false},
		{"Error with Spaces/Tabs", "- An error with\tspaces\tand newlines\r\n", '-', " An error with\tspaces\tand newlines", false},

		// --- Invalid Simple Errors (Malformed) ---
		{"No CRLF", "-ERR message", '-', nil, true},                         // Missing \r\n
		{"Only CR", "-ERR message\r", '-', nil, true},                       // Missing \n
		{"Only LF", "-ERR message\n", '-', nil, true},                       // Missing \r (or incorrect order)
		{"Missing Hyphen", "ERR message\r\n", 'E', nil, true},               // Does not start with '-'
		{"Contains CR in Message", "-ERR msg\rwith CR\r\n", '-', nil, true}, // Illegal \r in the error message content
		{"Contains LF in Message", "-ERR msg\nwith LF\r\n", '-', nil, true}, // Illegal \n in the error message content
		{"Empty input", "", 0, nil, true},                                   // No data
		{"Incomplete prefix", "-", '-', nil, true},                          // Just the prefix, no content or terminator
		{"Incomplete content", "-ERR", '-', nil, true},                      // Content but no terminator
		{"Trailing data", "-OK\r\n-next error\r\n", '-', "OK", true},        // Parser should expect exactly one simple error
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var dataType DataType
			var err error
			var panicked bool

			func() {
				defer func() {
					if r := recover(); r != nil {
						panicked = true
						if e, ok := r.(error); ok {
							err = e
						} else {
							err = fmt.Errorf("panic: %v", r)
						}
					}
				}()
				dataType, err = svc.Parse([]byte(tc.input))
			}()

			if tc.wantErr {
				if err == nil && !panicked {
					t.Errorf("Expected an error for input %q, but got none. Result: %+v", tc.input, dataType)
				}
			} else {
				if err != nil {
					t.Errorf("Did not expect an error for input %q, but got: %v", tc.input, err)
				}
				if panicked {
					t.Errorf("Unexpected panic for input %q: %v", tc.input, err)
				}
				if dataType.ID != tc.wantID {
					t.Errorf("ID mismatch for input %q: want %q, got %q", tc.input, rune(tc.wantID), rune(dataType.ID))
				}
				if !reflect.DeepEqual(dataType.ReturnType, tc.wantVal) {
					t.Errorf("Value mismatch for input %q: want %v (%T), got %v (%T)", tc.input, tc.wantVal, tc.wantVal, dataType.ReturnType, dataType.ReturnType)
				}
			}
			if t.Failed() {
				t.Logf("Failed: %s", tc.name)
				if err != nil {
					t.Logf("Actual Error: %v", err)
				}
			} else {
				t.Logf("Passed: %s", tc.name)
			}
		})
	}
}

// TestRespIntegerParser tests RESP Integer parsing.
func TestRespIntegerParser(t *testing.T) {
	svc := NewRespService() // Initialize your service

	tests := []struct {
		name    string
		input   string
		wantID  byte
		wantVal any // Expected int64 value
		wantErr bool
	}{
		// --- Valid Integers ---
		{"Zero", ":0\r\n", ':', int64(0), false},
		{"Positive Integer", ":123\r\n", ':', int64(123), false},
		{"Negative Integer", ":-456\r\n", ':', int64(-456), false},
		{"Large Positive Integer", ":9223372036854775807\r\n", ':', int64(9223372036854775807), false},   // Max int64
		{"Large Negative Integer", ":-9223372036854775808\r\n", ':', int64(-9223372036854775808), false}, // Min int64
		{"Explicit Positive Sign", ":+789\r\n", ':', int64(789), false},

		// --- Invalid Integers (Malformed) ---
		{"No CRLF", ":123", ':', nil, true},                     // Missing \r\n
		{"Only CR", ":123\r", ':', nil, true},                   // Missing \n
		{"Only LF", ":123\n", ':', nil, true},                   // Missing \r (or incorrect order)
		{"Missing Colon", "123\r\n", '1', nil, true},            // Does not start with ':'
		{"Non-numeric Characters", ":123a\r\n", ':', nil, true}, // Contains non-digits
		{"Non-numeric only", ":abc\r\n", ':', nil, true},        // Contains only non-digits
		{"Empty content", ":\r\n", ':', nil, true},              // Empty number
		{"Empty input", "", 0, nil, true},                       // No data
		{"Incomplete prefix", ":", ':', nil, true},              // Just the prefix, no content or terminator
		{"Incomplete content", ":12", ':', nil, true},           // Content but no terminator
		{"Trailing data", ":1\r\n:2\r\n", ':', int64(1), true},  // Parser should expect exactly one integer

		// --- Out of Range Integers (should error if Parse handles overflow) ---
		{"Overflow Positive", ":9223372036854775808\r\n", ':', nil, true},   // Max int64 + 1
		{"Underflow Negative", ":-9223372036854775809\r\n", ':', nil, true}, // Min int64 - 1
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var dataType DataType
			var err error
			var panicked bool

			func() {
				defer func() {
					if r := recover(); r != nil {
						panicked = true
						if e, ok := r.(error); ok {
							err = e
						} else {
							err = fmt.Errorf("panic: %v", r)
						}
					}
				}()
				dataType, err = svc.Parse([]byte(tc.input))
			}()

			if tc.wantErr {
				if err == nil && !panicked {
					t.Errorf("Expected an error for input %q, but got none. Result: %+v", tc.input, dataType)
				}
			} else {
				if err != nil {
					t.Errorf("Did not expect an error for input %q, but got: %v", tc.input, err)
				}
				if panicked {
					t.Errorf("Unexpected panic for input %q: %v", tc.input, err)
				}
				if dataType.ID != tc.wantID {
					t.Errorf("ID mismatch for input %q: want %q, got %q", tc.input, rune(tc.wantID), rune(dataType.ID))
				}
				if !reflect.DeepEqual(dataType.ReturnType, tc.wantVal) {
					t.Errorf("Value mismatch for input %q: want %v (%T), got %v (%T)", tc.input, tc.wantVal, tc.wantVal, dataType.ReturnType, dataType.ReturnType)
				}
			}
			if t.Failed() {
				t.Logf("Failed: %s", tc.name)
				if err != nil {
					t.Logf("Actual Error: %v", err)
				}
			} else {
				t.Logf("Passed: %s", tc.name)
			}
		})
	}
}

// TestRespBulkStringParser tests RESP Bulk String parsing.
func TestRespBulkStringParser(t *testing.T) {
	svc := NewRespService() // Initialize your service

	tests := []struct {
		name    string
		input   string
		wantID  byte
		wantVal any // Expected value for DataType.ReturnType
		wantErr bool
	}{
		// --- Valid Bulk Strings ---
		{"Basic bulk string", "$3\r\nfoo\r\n", '$', "foo", false},
		{"Empty bulk string", "$0\r\n\r\n", '$', "", false},
		{"Whitespace bulk string", "$3\r\n \t \r\n", '$', " \t ", false},
		{"Bulk string with CRLF", "$5\r\nfoo\r\n\r\n", '$', "foo\r\n", false},
		{"Null bulk string", "$-1\r\n", '$', nil, false},
		// --- Invalid Bulk Strings ---
		{"No CRLF at end", "$3\r\nfoo", '$', nil, true},
		{"No CR after length", "$3foo\r\n", '$', nil, true},
		{"No LF after CR", "$3\rfoo\r\n", '$', nil, true},
		{"Length mismatch (too short)", "$5\r\nfoo\r\n", '$', nil, true},
		{"Length mismatch (too long)", "$2\r\nfoo\r\n", '$', nil, true},
		{"Negative length not -1", "$-2\r\nfoo\r\n", '$', nil, true},
		{"Non-numeric length", "$abc\r\nfoo\r\n", '$', nil, true},
		{"Empty input", "", 0, nil, true},
		{"Only prefix", "$", '$', nil, true},
		{"Only length", "$3", '$', nil, true},
		{"Only length and CR", "$3\r", '$', nil, true},
		{"Only length and CRLF", "$3\r\n", '$', nil, true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var dataType DataType
			var err error
			var panicked bool

			func() {
				defer func() {
					if r := recover(); r != nil {
						panicked = true
						if e, ok := r.(error); ok {
							err = e
						} else {
							err = fmt.Errorf("panic: %v", r)
						}
					}
				}()
				dataType, err = svc.Parse([]byte(tc.input))
			}()

			if tc.wantErr {
				if err == nil && !panicked {
					t.Errorf("Expected an error for input %q, but got none. Result: %+v", tc.input, dataType)
				}
			} else {
				if err != nil {
					t.Errorf("Did not expect an error for input %q, but got: %v", tc.input, err)
				}
				if panicked {
					t.Errorf("Unexpected panic for input %q: %v", tc.input, err)
				}
				if dataType.ID != tc.wantID {
					t.Errorf("ID mismatch for input %q: want %q, got %q", tc.input, rune(tc.wantID), rune(dataType.ID))
				}
				if !reflect.DeepEqual(dataType.ReturnType, tc.wantVal) {
					t.Errorf("Value mismatch for input %q: want %v (%T), got %v (%T)", tc.input, tc.wantVal, tc.wantVal, dataType.ReturnType, dataType.ReturnType)
				}
			}
			if t.Failed() {
				t.Logf("Failed: %s", tc.name)
				if err != nil {
					t.Logf("Actual Error: %v", err)
				}
			} else {
				t.Logf("Passed: %s", tc.name)
			}
		})
	}
}

// TestRespArrayParser tests RESP Array parsing.
func TestRespArrayParser(t *testing.T) {
	svc := NewRespService() // Initialize your service

	tests := []struct {
		name    string
		input   string
		wantID  byte
		wantVal any // Expected value for DataType.BulkReturnType
		wantErr bool
	}{
		// --- Valid Arrays ---
		{"Empty array", "*0\r\n", '*', []any{}, false},
		{"Null array", "*-1\r\n", '*', nil, false},
		{"Array of simple strings", "*2\r\n+foo\r\n+bar\r\n", '*', []any{"foo", "bar"}, false},
		{"Array of integers", "*2\r\n:1\r\n:2\r\n", '*', []any{int64(1), int64(2)}, false},
		{"Array of bulk strings", "*2\r\n$3\r\nfoo\r\n$3\r\nbar\r\n", '*', []any{"foo", "bar"}, false},
		{"Array with null bulk string", "*2\r\n$-1\r\n$3\r\nbar\r\n", '*', []any{nil, "bar"}, false},
		{"Nested array", "*2\r\n*2\r\n+foo\r\n+bar\r\n:42\r\n", '*', []any{[]any{"foo", "bar"}, int64(42)}, false},
		{"Deeply nested array", "*2\r\n*1\r\n*1\r\n+foo\r\n:99\r\n", '*', []any{[]any{[]any{"foo"}}, int64(99)}, false},
		// --- Invalid Arrays ---
		{"No CRLF after header", "*2+foo\r\n+bar\r\n", '*', nil, true},
		{"Missing element", "*2\r\n+foo\r\n", '*', nil, true},
		{"Malformed nested array", "*1\r\n*2\r\n+foo\r\n", '*', nil, true},
		{"Non-numeric length", "*abc\r\n+foo\r\n", '*', nil, true},
		{"Negative length not -1", "*-2\r\n+foo\r\n", '*', nil, true},
		{"Empty input", "", 0, nil, true},
		{"Only prefix", "*", '*', nil, true},
		{"Only length", "*2", '*', nil, true},
		{"Only length and CR", "*2\r", '*', nil, true},
		{"Only length and CRLF", "*2\r\n", '*', nil, true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var dataType DataType
			var err error
			var panicked bool

			func() {
				defer func() {
					if r := recover(); r != nil {
						panicked = true
						if e, ok := r.(error); ok {
							err = e
						} else {
							err = fmt.Errorf("panic: %v", r)
						}
					}
				}()
				dataType, err = svc.Parse([]byte(tc.input))
			}()

			if tc.wantErr {
				if err == nil && !panicked {
					t.Errorf("Expected an error for input %q, but got none. Result: %+v", tc.input, dataType)
				}
			} else {
				if err != nil {
					t.Errorf("Did not expect an error for input %q, but got: %v", tc.input, err)
				}
				if panicked {
					t.Errorf("Unexpected panic for input %q: %v", tc.input, err)
				}
				if dataType.ID != tc.wantID {
					t.Errorf("ID mismatch for input %q: want %q, got %q", tc.input, rune(tc.wantID), rune(dataType.ID))
				}
				if !reflect.DeepEqual(dataType.ReturnType, tc.wantVal) {
					t.Errorf("Value mismatch for input %q: want %v (%T), got %v (%T)", tc.input, tc.wantVal, tc.wantVal, dataType.ReturnType, dataType.ReturnType)
				}
			}
			if t.Failed() {
				t.Logf("Failed: %s", tc.name)
				if err != nil {
					t.Logf("Actual Error: %v", err)
				}
			} else {
				t.Logf("Passed: %s", tc.name)
			}
		})
	}
}
