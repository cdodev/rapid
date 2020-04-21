variable "code_bucket" {
  # type = object
  description = "The S3 bucket for storing lambda functions"
}

variable "servant_lambda_function_name" {
  type = string
  description = "The name of the lambda function"
}

variable "servant_lambda_function_timeout" {
  type = number
  default = 30
  description = "The number of seconds the lambda will be allowed to run"
}

variable "servant_lambda_function_memory" {
  type = number
  default = 320
  description = "The amount of memory the lambda will be allowed"
}

variable "servant_lambda_handler" {
  type = string
  description = "The handler passed to the lambda function"
}

variable "servant_source_zip" {
  type = string
  default = "build/function.zip"
  description = "The local file containing the bootstrap executable"
}


variable "servant_rest_api_name" {
  type = string
  description = "The name of the API Gateway REST API"
}

variable "servant_rest_api_description" {
  type = string
  description = "The description of the API Gateway REST API"
}

variable "deployment_stages" {
  type = list(string)
  description = "List of stages in the API Gateway deployment"
  default = ["dev", "test", "staging", "production"]
}
