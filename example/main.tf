provider "aws" {
  region = "eu-west-1"
}


### Bucket for adding zipped functions to
resource "aws_s3_bucket" "example" {
  bucket = "rapid-example-code"
  acl    = "private"

  tags = {
    Name        = "Example of servant AWS Lambda"
    Environment = "Test"
  }
}


module "rapid_example" {
  source = "../terraform-module"

  code_bucket = aws_s3_bucket.example
  servant_lambda_function_name = "rapid_example"
  servant_lambda_handler = "example" # See ./app/bootstrap.hs
  servant_rest_api_name = "RapidExample"
  servant_rest_api_description = "Rapid Example"
}
