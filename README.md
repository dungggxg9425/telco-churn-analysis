# Phân tích tỷ lệ rời bỏ của khách hàng viễn thông (Telco Customer Churn)

Tìm hiểu nhóm khách hàng nào dễ rời bỏ dịch vụ và các yếu tố ảnh hưởng, bằng phân tích khám phá và mô hình hồi quy logistic trong R.

**Công cụ:** R (tidyverse, ggplot2, broom, pROC) · R Markdown
**Dữ liệu:** [IBM Telco Customer Churn](https://github.com/IBM/telco-customer-churn-on-icp4d) – 7.043 khách hàng, 21 cột (thông tin hợp đồng, dịch vụ đăng ký, cước phí, trạng thái rời bỏ).

📄 **Báo cáo đầy đủ (code + kết quả): [report.md](report.md)**

## Kết quả chính

- Tỷ lệ rời bỏ chung: **26,5%** (1.869 / 7.043 khách).
- **Loại hợp đồng tách biệt rõ nhất:** hợp đồng theo tháng rời bỏ **42,7%**, so với 11,3% (1 năm) và 2,8% (2 năm); kiểm định chi bình phương p < 2,2e-16. Sau khi kiểm soát các biến khác, hợp đồng 2 năm có odds rời bỏ chỉ bằng **0,22 lần** hợp đồng theo tháng.
- **Khách mới dễ rời bỏ nhất:** 47,4% ở nhóm dùng 0–12 tháng, so với 9,5% ở nhóm trên 4 năm.
- **Electronic check** có tỷ lệ rời bỏ **45,3%**, gần gấp 3 lần các hình thức thanh toán tự động (15,2–16,7%).
- **Internet cáp quang** làm tăng odds rời bỏ (OR = 1,63 so với DSL); có **Online Security** (OR = 0,64) hoặc **Tech Support** (OR = 0,67) làm giảm.
- **Mô hình hồi quy logistic:** AUC = **0,843** trên tập kiểm tra (30%). Ở ngưỡng 0,5: độ chính xác 79,8% (so với 73,5% khi đoán tất cả "không rời bỏ"), precision 64,2%, recall 53,7%.

| | |
|---|---|
| ![Theo hợp đồng](figures/fig-contract-tenure-1.png) | ![Theo thời gian sử dụng](figures/fig-contract-tenure-2.png) |
| ![Theo thanh toán](figures/fig-payment-1.png) | ![Tỷ số odds](figures/fig-odds-1.png) |

## Đề xuất

1. Ưu đãi chuyển khách hợp đồng theo tháng sang hợp đồng 1–2 năm, tập trung vào 12 tháng đầu.
2. Khuyến khích chuyển từ Electronic check sang thanh toán tự động.
3. Tặng dùng thử Online Security / Tech Support cho khách dùng Internet cáp quang.

## Quy trình

1. **Làm sạch:** 11 khách mới (tenure = 0) thiếu `TotalCharges` → gán 0; gộp giá trị "No internet service" / "No phone service" vào "No" để tránh trùng thông tin với `InternetService`; nhóm `tenure` thành 4 khoảng.
2. **Phân tích khám phá:** tỷ lệ rời bỏ theo hợp đồng, thời gian sử dụng, phương thức thanh toán, dịch vụ Internet; kiểm định chi bình phương.
3. **Mô hình:** chia 70/30 có phân tầng (`set.seed(42)`); hồi quy logistic với 9 biến; bỏ `TotalCharges` do đa cộng tuyến với `tenure × MonthlyCharges`; báo cáo tỷ số odds và khoảng tin cậy 95%.
4. **Đánh giá:** AUC, ma trận nhầm lẫn, accuracy, precision, recall; so sánh với mô hình cơ sở.

## Chạy lại

```r
install.packages(c("tidyverse", "broom", "pROC", "rmarkdown"))
```

```bash
Rscript R/render.R     # tạo report.md, figures/ và outputs/
```

## Cấu trúc thư mục

```
├── data/        # Telco-Customer-Churn.csv
├── R/           # analysis.Rmd (phân tích), render.R
├── figures/     # biểu đồ
├── outputs/     # tỷ lệ rời bỏ theo nhóm, tỷ số odds, chỉ số mô hình
└── report.md    # báo cáo đầy đủ
```

---
Tác giả: **Nguyễn Duy Hiếu** – sinh viên Khoa học dữ liệu, UNETI · [LinkedIn](https://www.linkedin.com/in/dungggxg9425) · [GitHub](https://github.com/dungggxg9425)
