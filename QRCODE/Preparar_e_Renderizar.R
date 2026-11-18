# GERAR O QR CODE --------------------------------------------

if (!requireNamespace("qrcode", quietly = TRUE)) {
  install.packages("qrcode", repos = "https://cloud.r-project.org")
}

# Use este endereço se o GitHub Pages publicar a pasta QRCODE
# do repositório Lucasbpofc.github.io.
url_analise <- "https://lucasbpofc.github.io/QRCODE/"

qr <- qrcode::qr_code(url_analise)

# PDF vetorial para incluir no pôster pelo Overleaf
grDevices::pdf(
  "qrcode_analise.pdf",
  width = 4,
  height = 4,
  bg = "white"
)
plot(qr)
grDevices::dev.off()

message("QR code salvo em: qrcode_analise.pdf")
message("Endereço utilizado: ", url_analise)