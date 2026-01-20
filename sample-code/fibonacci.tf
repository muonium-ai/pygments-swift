# Terraform / HCL sample
variable "n" {
  type    = number
  default = 10
}

output "fib" {
  value = "${var.n}"
}

resource "null_resource" "example" {
  triggers = {
    n = var.n
  }
}
