# resp-parser-go
Implementation of a simple RESP (Redis Serialization Protocol) parser in Golang 

# Example 
```go
package main

import (
	"fmt"
	"log"
	// Assuming your resp.go file is in the same package or is imported 
)

func main() {
	// Initialize your RESP parser service
	svc := NewRespService()

	// Example: Parse a simple string "+OK\r\n"
	input := []byte("+OK\r\n")
	parsedData, err := svc.Parse(input)
	if err != nil {
		log.Fatalf("Error parsing RESP data: %v", err)
	}

	fmt.Printf("Parsed ID: %c\n", parsedData.ID)
	fmt.Printf("Parsed ReturnType: %v\n", parsedData.ReturnType)
	fmt.Printf("Parsed Raw Message: %q\n", parsedData.Msg)

	// Example: Parse an integer ":123\r\n"
	inputInt := []byte(":123\r\n")
	parsedIntData, err := svc.Parse(inputInt)
	if err != nil {
		log.Fatalf("Error parsing RESP integer: %v", err)
	}

	fmt.Printf("Parsed Integer ID: %c\n", parsedIntData.ID)
	fmt.Printf("Parsed Integer ReturnType: %v\n", parsedIntData.ReturnType)
	fmt.Printf("Parsed Integer Raw Message: %q\n", parsedIntData.Msg)

	// Example: Parse an array "*2\r\n+foo\r\n:123\r\n"
	inputArray := []byte("*2\r\n+foo\r\n:123\r\n")
	parsedArrayData, err := svc.Parse(inputArray)
	if err != nil {
		log.Fatalf("Error parsing RESP array: %v", err)
	}

	fmt.Printf("Parsed Array ID: %c\n", parsedArrayData.ID)
	// For arrays, the parsed elements are in BulkReturnType
	fmt.Printf("Parsed Array Elements: %v\n", parsedArrayData.BulkReturnType) 
	fmt.Printf("Parsed Array Raw Message: %q\n", parsedArrayData.Msg)
}```

# Test
```zsh
    go test -v
```