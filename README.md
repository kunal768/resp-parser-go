# resp-parser-go
Implementation of a simple RESP (Redis Serialization Protocol) parser in Golang <br />
>[!IMPORTANT]  
>Currently supports RESP2 only

# To Import 
```bash
go get github.com/kunal768/resp-parser-go/resp
```

# Example 
```go
package main

import (
	"fmt"
	"log"

	"github.com/kunal768/resp-parser-go/resp"
)

func main() {
	// Initialize your RESP parser service
	svc := resp.NewRespService()

	/* Parse examples */

	// Example: Parse a simple string "+OK\r\n"
	input := []byte("+OK\r\n")
	parsedData, err := svc.Parse(input)
	if err != nil {
		log.Fatalf("Error parsing RESP data: %v", err)
	}

	fmt.Printf("Parsed ID: %c\n", parsedData.ID)              // +
	fmt.Printf("Parsed ReturnType: %v\n", parsedData.ReturnType) // OK
	fmt.Printf("Parsed Raw Message: %q\n", parsedData.Msg)       // "+OK\r\n"

	// Example: Parse an integer ":123\r\n"
	inputInt := []byte(":123\r\n")
	parsedIntData, err := svc.Parse(inputInt)
	if err != nil {
		log.Fatalf("Error parsing RESP integer: %v", err)
	}

	fmt.Printf("Parsed Integer ID: %c\n", parsedIntData.ID)              // :
	fmt.Printf("Parsed Integer ReturnType: %v\n", parsedIntData.ReturnType) // 123
	fmt.Printf("Parsed Integer Raw Message: %q\n", parsedIntData.Msg)       // ":123\r\n"

	// Example: Parse an array "*2\r\n+foo\r\n:123\r\n"
	inputArray := []byte("*2\r\n+foo\r\n:123\r\n")
	parsedArrayData, err := svc.Parse(inputArray)
	if err != nil {
		log.Fatalf("Error parsing RESP array: %v", err)
	}

	fmt.Printf("Parsed Array ID: %c\n", parsedArrayData.ID) // *
	// For arrays, the parsed elements are in BulkReturnType
	fmt.Printf("Parsed Array Elements: %v\n", parsedArrayData.BulkReturnType) // [foo 123]
	fmt.Printf("Parsed Array Raw Message: %q\n", parsedArrayData.Msg) // "*2\r\n+foo\r\n:123\r\n"

	/* Serialize examples */

	// Simple string
	simpleStr := resp.DataType{ID: resp.STRING, ReturnType: "OK"}
	result, _ := svc.Serialize(simpleStr) // "+OK\r\n"
	fmt.Println(result)

	// Integer
	integer := resp.DataType{ID: resp.INTEGER, ReturnType: int64(42)}
	result, _ = svc.Serialize(integer) // ":42\r\n"
	fmt.Println(result)

	// Bulk string
	bulkStr := resp.DataType{ID: resp.BULK, ReturnType: "Hello World"}
	result, _ = svc.Serialize(bulkStr) // "$11\r\nHello World\r\n"
	fmt.Println(result)

	// Array
	array := resp.DataType{
		ID:             resp.ARRAY,
		BulkReturnType: []any{"GET", "key", int64(42)},
	}
	result, _ = svc.Serialize(array) // "*3\r\n$3\r\nGET\r\n$3\r\nkey\r\n:42\r\n"
	fmt.Println(result)
}
```

>[!CAUTION]  
>For arrays, the parsed elements are in `BulkReturnType`

# Run Tests
```zsh
go test -v ./resp
```
