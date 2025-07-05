# TCG Document Writer Service

This service provides an API for generating text responses based on user queries, leveraging a Large Language Model (LLM) and a Vector Database (VDB) for enhanced context retrieval and response quality.

## Endpoints

### 1. `/genie` (POST)

This is the primary endpoint for generating text responses. It accepts a JSON payload containing a user query and configuration parameters, and returns a JSON response with the generated text.

#### Request

- **Method:** POST
- **Path:** `/genie`
- **Headers:**
  - `Content-Type`: `application/json`
- **Body:** A JSON object representing a `FrontEndThread` (defined in `ApiCore.hs`).

  ```json
    {
    "thread_id": "thread_12345",
    "messages": [
        {
        "message_id": "msg1",
        "role": "user",
        "message": "幫我寫一篇關於小額採購雷射筆用於簡報使用，並預計花費約新台幣十萬元。",
        "rewrite_flag": true
        }
    ],
    "vdb_config": {
        "collection": "Documents",
        "top_k": 2,
        "pool_size": 10,
        "alpha": 0.7
    },
    "generator_config": {
        "model": "gpt-4",
        "temperature": 0.8,
        "top_p": 0.95,
        "frequency_penalty": 0.1,
        "presence_penalty": 0.1
    }
    }
  ```

  - `messages`: An array of message objects, each with:
    - `message_id`: A unique identifier for the message.
    - `role`: The role of the message sender ("user" or "assistant").
    - `message`: The content of the message.
    - `rewrite_flag`: A boolean indicating whether the message should be rewritten using the prompt rewriter.
  - `vdb_config`: Configuration for the vector database:
    - `collection`: The name of the collection to search.
    - `top_k`: The number of top results to retrieve.
    - `pool_size`: The number of documents to retrieve from the wider pool.
    - `alpha`: The alpha parameter for hybrid search.
  - `generator_config`: Configuration for the LLM:
    - `model`: The model to use (e.g., "gpt-3.5-turbo"). (**NOTE**: Model selection is not available in the current version and will be ignored.)
    - `temperature`: The temperature for sampling.
    - `top_p`: The top-p sampling parameter.
    - `frequency_penalty`: The frequency penalty.
    - `presence_penalty`: The presence penalty.
  - `thread_id`: A unique identifier for the conversation thread.

#### Response

- **Status:** 200 OK
- **Headers:**
  - `Content-Type`: `application/json`
- **Body:** A JSON object representing a `FrontEndMessage` with the generated response.

  ```json
    {
    "message_id": "assistant-msg1",
    "role": "assistant",
    "message": "為辦理本局簡報展示所需雷射筆小額採購一案，簽請核示。...",
    "rewrite_flag": false
    }
  ```

### 2. `/health` (GET)

This endpoint provides a simple health check, returning a 200 OK status if the service is running.

#### Request

- **Method:** GET
- **Path:** `/health`

#### Response

- **Status:** 200 OK
- **Body:** "OK"

## Data Types

The key data types used in the API are defined in `ApiCore.hs`:

- `ApiGeneratorRequest`: Configuration for the LLM.
- `ApiVdbConfig`: Configuration for the vector database.
- `ApiRequest`: Represents a complete API request.
- `FrontEndMessage`: Represents a single message in a conversation.
- `FrontEndThread`: Represents a thread of messages.

See the `ApiCore.hs` file for detailed definitions of these types.

## Testing

You may test the API using tools like `curl` or Postman. For example, to test the `/genie` endpoint, you can use the following `curl` command:

```bash
curl -X POST -H "Content-Type: application/json" -d '{"thread_id":"thread_12345","messages":[{"message_id":"msg1","role":"user","message":"幫我寫一篇關於小額採購雷射筆用於簡報使用，並預計花費約新台幣十萬元。","rewrite_flag":true}],"vdb_config":{"collection":"Documents","top_k":2,"pool_size":10,"alpha":0.7},"generator_config":{"model":"gpt-4","temperature":0.8,"top_p":0.95,"frequency_penalty":0.1,"presence_penalty":0.1}}' http://127.0.0.1:8080/genie
```