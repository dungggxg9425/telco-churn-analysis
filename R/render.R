# Render báo cáo R Markdown ra report.md ở thư mục gốc dự án.
# Chạy từ thư mục gốc: Rscript R/render.R
invisible(Sys.setlocale("LC_ALL", "C.UTF-8"))   # hiển thị đúng tiếng Việt trên biểu đồ
setwd("R")
rmarkdown::render("analysis.Rmd", output_file = "../report.md", quiet = TRUE)
setwd("..")
# Ảnh được lưu ở figures/ (đường dẫn tương đối từ R/ là ../figures/) -> sửa lại cho report.md ở thư mục gốc
md <- readLines("report.md", encoding = "UTF-8")
writeLines(gsub("](../figures/", "](figures/", md, fixed = TRUE), "report.md", useBytes = TRUE)
cat("Đã tạo report.md\n")
