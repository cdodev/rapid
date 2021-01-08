### Zip file containing the haskell boostrap executable see
# Makefile for how it's generated
resource "aws_s3_bucket_object" "lambda_code" {
  bucket = var.code_bucket.id
  key    = "function.zip"
  source = var.servant_source_zip

  # The filemd5() function is available in Terraform 0.11.12 and later
  # For Terraform 0.11.11 and earlier, use the md5() function and the file() function:
  # etag = "${md5(file("path/to/file"))}"
  etag = filemd5("${var.servant_source_zip}")
}

### AWSServant module. See app/bootstrap for routing
resource "aws_lambda_function" "servant" {
  s3_bucket        = aws_s3_bucket_object.lambda_code.bucket
  s3_key           = "function.zip"
  function_name    = var.servant_lambda_function_name
  role             = aws_iam_role.servant_iam.arn
  handler          = var.servant_lambda_handler
  source_code_hash = filebase64sha256("${var.servant_source_zip}")
  timeout          = var.servant_lambda_function_timeout
  memory_size      = var.servant_lambda_function_memory
  runtime          = "provided"
}

### GATEWAY CONFIG
# https://ordina-jworks.github.io/cloud/2019/01/14/Infrastructure-as-code-with-terraform-and-aws-serverless.html
resource "aws_api_gateway_rest_api" "servant" {
  name        = var.servant_rest_api_name
  description = var.servant_rest_api_description
  # Use the swagger file generated in by the stack exec -- generate-swagger step in the make file
  # This is auto generated from the types and ToSchema instances in the servant API
  body = data.template_file.servant_api_swagger.rendered
}

data "template_file" "servant_api_swagger" {
  template = file("swagger.json")

  # Fill in the servant_lamba_arn value in the servant template (see the ToJSON
  # encoding of the XAmazonGatewayIntegration uri field) in the Swagger.hs module
  vars = {
    servant_lambda_arn = "${aws_lambda_function.servant.invoke_arn}"
  }
}

## /docs endpoint resource
# Add a resource to route the /docs endpoint and swagger file to the servant lamba
resource "aws_api_gateway_resource" "swagger-docs" {
  rest_api_id = aws_api_gateway_rest_api.servant.id
  parent_id   = aws_api_gateway_rest_api.servant.root_resource_id
  path_part   = "{proxy+}"
}

resource "aws_api_gateway_method" "method" {
  rest_api_id   = aws_api_gateway_rest_api.servant.id
  resource_id   = aws_api_gateway_resource.swagger-docs.id
  http_method   = "GET"
  authorization = "NONE"
}
## /docs endpoint resource ends

resource "aws_api_gateway_integration" "servant" {
  rest_api_id             = aws_api_gateway_rest_api.servant.id
  resource_id             = aws_api_gateway_resource.swagger-docs.id
  http_method             = aws_api_gateway_method.method.http_method
  integration_http_method = "POST"
  type                    = "AWS_PROXY"
  uri                     = aws_lambda_function.servant.invoke_arn
}

resource "aws_api_gateway_deployment" "servant" {
  depends_on        = [aws_api_gateway_integration.servant, aws_api_gateway_rest_api.servant]
  rest_api_id       = aws_api_gateway_rest_api.servant.id
  stage_name        = "live"
  stage_description = md5(file("swagger.json"))
  lifecycle {
    create_before_destroy = true
  }
}
### GATEWAY CONFIG ENDS


### BOILERPLATE
# Role

resource "aws_iam_policy" "servant_logging" {
  name        = "${var.servant_lambda_function_name}_logging"
  path        = "/"
  description = "IAM policy for logging from a lambda"

  policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": [
        "logs:CreateLogGroup",
        "logs:CreateLogStream",
        "logs:PutLogEvents"
      ],
      "Resource": "arn:aws:logs:*:*:*",
      "Effect": "Allow"
    }
  ]
}
EOF
}


resource "aws_iam_role" "servant_iam" {
  name = "${var.servant_lambda_function_name}-iam"

  assume_role_policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "lambda.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    }
  ]
}
EOF
}

resource "aws_iam_role_policy_attachment" "lambda_logs" {
  role       = aws_iam_role.servant_iam.name
  policy_arn = aws_iam_policy.servant_logging.arn
}


# Permission
resource "aws_lambda_permission" "apigw_lambda" {
  statement_id  = "AllowExecutionFromAPIGateway"
  action        = "lambda:InvokeFunction"
  function_name = aws_lambda_function.servant.function_name
  principal     = "apigateway.amazonaws.com"

  # More: http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-control-access-using-iam-policies-to-invoke-api.html
  source_arn = "${aws_api_gateway_rest_api.servant.execution_arn}/*/*/*"
}
