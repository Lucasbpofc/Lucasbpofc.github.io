library(qrcode)

# Se não, cole o link COMPLETO do shinyapps (cuidado com espaços extras)
meu_link_final <- "https://lucasbpofc.shinyapps.io/SURPRESA/" 

# 1. Gerar o objeto do QR Code
code <- qr_code(meu_link_final)

# 2. Salvar como imagem PNG (Mais fácil de usar que SVG)
# Isso vai criar um arquivo "qrcode_amor.png" na sua pasta
png("qrcode_amor.png", width = 1000, height = 1000)
plot(code)
dev.off() # Fecha o arquivo para salvar

# 3. Mostrar na tela do RStudio para você testar agora
plot(code)
