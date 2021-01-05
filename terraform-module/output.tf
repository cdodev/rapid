output "url" {
  value       = aws_api_gateway_deployment.servant.invoke_url
  description = "The invoke url for the API Gateway endpoint"
}

output "lambda_code_object" {
  value       = aws_s3_bucket_object.lambda_code
  description = "The bucket object containing the fat lambda (in case more handlers are defined)"
}

output "lambda_iam_role" {
  value       = aws_iam_role.servant_iam
  description = "The bucket object containing the fat lambda (in case more handlers are defined)"
}
