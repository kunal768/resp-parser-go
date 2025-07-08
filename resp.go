/*
Title  : A simple RESP (Redis Serialization Protocol) Parser implementation in Go
Author : Kunal Keshav Singh Sahni
Github : https://www.github.com/kunal768/resp-parser-go
Note   : Currently only supports RESP2 specification
TODO   : Add support for RESP3
*/

// INFO (Source : https://redis.io/docs/latest/develop/reference/protocol-spec/)
/*
	Redis serialization protocol (RESP) is the wire protocol that clients implement
	To communicate with the Redis server, Redis clients use a protocol called Redis Serialization Protocol (RESP)

	RESP is a compromise among the following considerations:
		Simple to implement.
		Fast to parse.
		Human readable.

	RESP can serialize different data types including integers, strings, and arrays.
	It also features an error-specific type. A client sends a request to the Redis server as an array of strings.
	The array's contents are the command and its arguments that the server should execute.
	The server's reply type is command-specific.

	RESP is binary-safe and uses prefixed length to transfer bulk data so it does not require processing bulk data transferred
	from one process to another.

*/

package main

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"
	"math"
	"strconv"
	"strings"
)

const (
	INTEGER byte = 58
	STRING  byte = 43
	BULK    byte = 36
	ARRAY   byte = 42
	ERROR   byte = 45
	CR      byte = 13
	LF      byte = 10
)

const megabyte = 1024 * 1024

type DataType struct {
	ID             byte   // ID or identifier is the first byte identifier of the RESP string
	Msg            []byte // The byte data which is contained in the original REDIS string or which will be contained in returning command
	ReturnType     any    // For RESP2 can be string, error string, int64, bulk string
	BulkReturnType []any  // To handle arrays
}

var (
	emptyInputError                  = errors.New("input data is empty")
	invalidInputError                = errors.New("input data is invalid")
	invalidInputForSimpleStringError = errors.New("input data is invalid for simple string")
	invalidInputForSimpleErrorError  = errors.New("input data is invalid for simple error")
	invalidInputForIntegerError      = errors.New("invalid input for integer error")
	invalidInputIDError              = errors.New("invalid ID for input error")
	invalidBulkStringInputError      = errors.New("invalid bulk string input error")
)

type Svc struct {
}

func NewRespService() *Svc {
	return &Svc{}
}

type RedisSerializationProtocolParser interface {
	Parse(data []byte) (DataType, error)
}

func (s *Svc) Parse(data []byte) (DataType, error) {
	if len(data) == 0 {
		fmt.Println("error while parsing: ", emptyInputError.Error())
		return DataType{}, emptyInputError
	}

	if len(data) < 3 {
		fmt.Println("error while parsing: ", invalidInputError.Error())
		return DataType{}, invalidInputError
	}

	if !isKnownType(data[0]) {
		fmt.Println("error while parsing: ", invalidInputIDError.Error())
		return DataType{}, invalidInputIDError
	}

	dataType := DataType{
		ID:  data[0],
		Msg: data,
	}

	err := s.handleParsing(dataType.ID)(&dataType)
	if err != nil {
		fmt.Println("error while parsing: ", err.Error())
		return DataType{}, err
	}

	if dataType.ID == ARRAY {
		if dataType.BulkReturnType == nil {
			dataType.ReturnType = nil
		} else {
			dataType.ReturnType = dataType.BulkReturnType
		}
	}

	return dataType, nil
}

/* RESP2 support */

/*
	Supports the following RESP2 data types:

	| RESP data type  | Minimal protocol version | Category   | First byte |
	|-----------------|--------------------------|------------|------------|
	| Simple strings  | RESP2                    | Simple     | +          |
	| Simple Errors   | RESP2                    | Simple     | -          |
	| Integers        | RESP2                    | Simple     | :          |
	| Bulk strings    | RESP2                    | Aggregate  | $          |
	| Arrays          | RESP2                    | Aggregate  | *          |

*/

/*
	RESP data type : Simple Strings
	Minimum Protocol Version: RESP2
	Category : Simple
	First Byte: "+"
	Link: https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-strings
*/

func (s *Svc) ParseToSimpleString(dataType *DataType) error {
	return s.parseSimpleType(dataType, invalidInputForSimpleStringError)
}

/*
	RESP data type : Simple Errors
	Minimum Protocol Version: RESP2
	Category : Simple
	First Byte: "-"
	Link: https://redis.io/docs/latest/develop/reference/protocol-spec/#simple-errors
*/

func (s *Svc) ParseToSimpleError(dataType *DataType) error {
	return s.parseSimpleType(dataType, invalidInputForSimpleErrorError)
}

/*
	RESP data type : Integers
	Minimum Protocol Version: RESP2
	Category : Simple
	First Byte: ":"
	Link: https://redis.io/docs/latest/develop/reference/protocol-spec/#integers
*/

func (s *Svc) ParseToInteger(dataType *DataType) error {
	var rd *bufio.Reader
	signByte := dataType.Msg[1]
	var sign int64 = 1     // sign of integer
	if signByte == ERROR { // -ve
		sign *= -1
	}
	if signByte == STRING || signByte == ERROR { // +ve or -ve
		rd = bufio.NewReader(bytes.NewReader(dataType.Msg[2:]))
	} else {
		rd = bufio.NewReader(bytes.NewReader(dataType.Msg[1:]))
	}

	var res int64
	foundDigit := false

	err := s.readWithCRLFValidation(rd, func(b byte) error {
		// validate byte for digits
		if b < 48 || b > 57 {
			fmt.Println("error : ", invalidInputForIntegerError.Error())
			return invalidInputForIntegerError
		}

		if !foundDigit {
			foundDigit = true
		}

		if sign > 0 {
			// check for overflow
			if res > (math.MaxInt64-int64(b-48))/10 {
				fmt.Println("error : ", invalidInputForIntegerError.Error())
				return invalidInputForIntegerError
			}
			// write to result
			res *= 10
			res += int64(b - 48)

		} else {
			// check for underflow
			if res < (math.MinInt64+int64(b-48))/10 {
				fmt.Println("error : ", invalidInputForIntegerError.Error())
				return invalidInputForIntegerError
			}
			// write to result
			res *= 10
			res -= int64(b - 48)
		}
		return nil
	}, invalidInputForIntegerError)

	if err != nil {
		return err
	}

	if !foundDigit {
		return invalidInputForIntegerError
	}

	dataType.ReturnType = res
	return nil
}

/*
	RESP data type : Bulk strings
	Minimum Protocol Version: RESP2
	Category : Aggregate
	First Byte: "$"
	Link: https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings
*/

func (s *Svc) ParseToBulkString(dataType *DataType) error {
	// validate input
	size := len(dataType.Msg) // max size of the bulk string is 512MB
	if size < 4 || size > 512*megabyte {
		fmt.Println("error : ", invalidBulkStringInputError.Error())
		return invalidBulkStringInputError
	}

	// checks first \r
	startingIdx := bytes.IndexByte(dataType.Msg, CR) // first index where \r is encountered
	if startingIdx == -1 {
		fmt.Println("error : ", invalidBulkStringInputError.Error())
		return invalidBulkStringInputError
	}

	// checks for \n after \r
	if startingIdx+1 > size-1 || dataType.Msg[startingIdx+1] != LF {
		fmt.Println("error : ", invalidBulkStringInputError.Error())
		return invalidBulkStringInputError
	}

	// size is from first index till the starting index check if its a valid number
	stringLength, ok := isNumber(string(dataType.Msg[1:startingIdx]))
	if !ok {
		fmt.Println("error : ", invalidBulkStringInputError.Error())
		return invalidBulkStringInputError
	}

	// special case string length : -1 (null bulk string)
	if stringLength == -1 {
		dataType.ReturnType = nil
		return nil
	}
	if stringLength < 0 {
		return invalidBulkStringInputError
	}

	bulkStart := startingIdx + 2
	bulkEnd := bulkStart + int(stringLength)
	if bulkEnd+2 > size {
		fmt.Println("error : ", invalidBulkStringInputError.Error())
		return invalidBulkStringInputError
	}
	// check if bulk string ends with \r\n after the data
	if string(dataType.Msg[bulkEnd:bulkEnd+2]) != "\r\n" {
		fmt.Println("error : ", invalidBulkStringInputError.Error())
		return invalidBulkStringInputError
	}

	bulkString := string(dataType.Msg[bulkStart:bulkEnd])
	dataType.ReturnType = bulkString
	return nil
}

/*
	RESP data type : Arrays
	Minimum Protocol Version: RESP2
	Category : Aggregate
	First Byte: "*"
	Link: https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays
*/

// Helper to parse any RESP value from a given position, returns value, new position, error
func (s *Svc) parseRESPValue(msg []byte, pos int) (any, int, error) {
	if pos >= len(msg) {
		return nil, pos, invalidInputError
	}
	typeByte := msg[pos]
	switch typeByte {
	case STRING, ERROR, INTEGER:
		crlf := bytes.Index(msg[pos:], []byte("\r\n"))
		if crlf == -1 {
			return nil, pos, invalidInputError
		}
		elemMsg := msg[pos : pos+crlf+2]
		var elem DataType
		elem.ID = typeByte
		elem.Msg = elemMsg
		err := s.handleParsing(typeByte)(&elem)
		if err != nil {
			return nil, pos, err
		}
		return elem.ReturnType, pos + crlf + 2, nil
	case BULK:
		bulkCr := bytes.IndexByte(msg[pos:], CR)
		if bulkCr == -1 {
			return nil, pos, invalidBulkStringInputError
		}
		bulkLen, ok := isNumber(string(msg[pos+1 : pos+bulkCr]))
		if !ok {
			return nil, pos, invalidBulkStringInputError
		}
		bulkStart := pos + bulkCr + 2
		if bulkLen == -1 {
			// Null bulk string: $-1\r\n
			if bulkStart > len(msg) {
				return nil, pos, invalidBulkStringInputError
			}
			// $-1\r\n is 5 bytes: $ - 1 \r \n
			return nil, bulkStart, nil
		}
		bulkEnd := bulkStart + int(bulkLen)
		if bulkEnd+2 > len(msg) {
			return nil, pos, invalidBulkStringInputError
		}
		elemMsg := msg[pos : bulkEnd+2]
		var elem DataType
		elem.ID = BULK
		elem.Msg = elemMsg
		err := s.ParseToBulkString(&elem)
		if err != nil {
			return nil, pos, err
		}
		return elem.ReturnType, bulkEnd + 2, nil
	case ARRAY:
		arr, newPos, err := s.parseRESPArray(msg, pos)
		return arr, newPos, err
	default:
		return nil, pos, invalidInputIDError
	}
}

// Helper to parse an array from a given position, returns []any, new position, error
func (s *Svc) parseRESPArray(msg []byte, pos int) ([]any, int, error) {
	if pos >= len(msg) || msg[pos] != ARRAY {
		return nil, pos, invalidInputIDError
	}
	crIdx := bytes.IndexByte(msg[pos:], CR)
	if crIdx == -1 || pos+crIdx+1 >= len(msg) || msg[pos+crIdx+1] != LF {
		return nil, pos, invalidInputError
	}
	lenStr := string(msg[pos+1 : pos+crIdx])
	arrLen, ok := isNumber(lenStr)
	if !ok {
		return nil, pos, invalidInputError
	}
	if arrLen == -1 {
		return nil, pos + crIdx + 2, nil // Null array
	}
	if arrLen < -1 {
		return nil, pos, invalidInputError
	}
	if arrLen == 0 {
		return []any{}, pos + crIdx + 2, nil
	}
	var elements []any
	cur := pos + crIdx + 2
	for i := int64(0); i < arrLen; i++ {
		if cur >= len(msg) {
			return nil, cur, invalidInputError
		}
		val, next, err := s.parseRESPValue(msg, cur)
		if err != nil {
			return nil, cur, err
		}
		elements = append(elements, val)
		cur = next
	}
	return elements, cur, nil
}

func (s *Svc) ParseToArray(dataType *DataType) error {
	arr, _, err := s.parseRESPArray(dataType.Msg, 0)
	if err != nil {
		return err
	}
	if arr == nil {
		// Null array
		dataType.BulkReturnType = nil
	} else {
		dataType.BulkReturnType = arr
	}
	return nil
}

/* Helper Methods */

// parseSimpleType handles the common parsing logic for simple strings and errors
func (s *Svc) parseSimpleType(dataType *DataType, invalidError error) error {
	rd := bufio.NewReader(bytes.NewReader(dataType.Msg[1:]))
	var res strings.Builder

	err := s.readWithCRLFValidation(rd, func(b byte) error {
		// write to result
		res.WriteString(string(b))
		return nil
	}, invalidError)

	if err != nil {
		return err
	}

	dataType.ReturnType = res.String()
	return nil
}

// readWithCRLFValidation reads from a reader with CRLF validation and calls the provided handler for each byte
func (s *Svc) readWithCRLFValidation(rd *bufio.Reader, handler func(byte) error, invalidError error) error {
	buffer := make([]byte, 5) // read 5 bytes at a time
	foundLF, foundCR := false, false

	for {
		size, err := rd.Read(buffer)
		if err != nil {
			if err == io.EOF {
				//TODO: ignore for now
				break
			}
			fmt.Println("error while parsing : ", err.Error())
			return err
		}

		for i := range size {
			b := buffer[i]

			// if found more characters after \r\n
			if foundLF && foundCR {
				fmt.Println("error : ", invalidError.Error())
				return invalidError
			}

			if foundCR && b == CR {
				fmt.Println("error : ", invalidError.Error())
				return invalidError
			}

			if foundLF && b == LF {
				fmt.Println("error : ", invalidError.Error())
				return invalidError
			}

			if !foundCR && b == CR {
				foundCR = true
			}

			if !foundLF && b == LF {
				foundLF = true
			}

			// \n appears before \r
			if foundLF && !foundCR {
				fmt.Println("error : ", invalidError.Error())
				return invalidError
			}

			// ignore LF/CR
			if b == LF || b == CR {
				continue
			}

			// call the handler for processing the byte
			if err := handler(b); err != nil {
				return err
			}
		}
	}

	if !foundCR || !foundLF {
		return invalidError
	}

	return nil
}

func (s *Svc) handleParsing(dataTypeId byte) func(*DataType) error {
	switch dataTypeId {
	case STRING:
		return s.ParseToSimpleString
	case ERROR:
		return s.ParseToSimpleError
	case INTEGER:
		return s.ParseToInteger
	case BULK:
		return s.ParseToBulkString
	case ARRAY:
		return s.ParseToArray
	default:
		return func(*DataType) error { return invalidInputError }
	}
}

func isKnownType(b byte) bool {
	switch b {
	case INTEGER, STRING, BULK, ARRAY, ERROR:
		return true
	default:
		return false
	}
}

func isNumber(s string) (int64, bool) {
	if len(s) == 0 {
		return math.MinInt64, false
	}
	num, err := strconv.ParseInt(s, 10, 64)
	if err != nil {
		return math.MinInt64, false
	}
	return num, true
}
