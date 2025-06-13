#!/bin/bash

# Hàm bỏ dấu tiếng Việt bằng thay thế thủ công
remove_diacritics() {
  echo "$1" \
  | sed -e 's/[áàảãạâấầẩẫậăắằẳẵặ]/a/g' \
        -e 's/[éèẻẽẹêếềểễệ]/e/g' \
        -e 's/[íìỉĩị]/i/g' \
        -e 's/[óòỏõọôốồổỗộơớờởỡợ]/o/g' \
        -e 's/[úùủũụưứừửữự]/u/g' \
        -e 's/[ýỳỷỹỵ]/y/g' \
        -e 's/đ/d/g' \
        -e 's/[ÁÀẢÃẠÂẤẦẨẪẬĂẮẰẲẴẶ]/A/g' \
        -e 's/[ÉÈẺẼẸÊẾỀỂỄỆ]/E/g' \
        -e 's/[ÍÌỈĨỊ]/I/g' \
        -e 's/[ÓÒỎÕỌÔỐỒỔỖỘƠỚỜỞỠỢ]/O/g' \
        -e 's/[ÚÙỦŨỤƯỨỪỬỮỰ]/U/g' \
        -e 's/[ÝỲỶỸỴ]/Y/g' \
        -e 's/Đ/D/g'
}

# Duyệt file trong thư mục
for file in *; do
  [ -f "$file" ] || continue

  # Bỏ dấu
  clean_name=$(remove_diacritics "$file")

  # Đổi chữ thường và khoảng trắng thành _
  new_name=$(echo "$clean_name" | tr '[:upper:]' '[:lower:]' | tr ' ' '_')

  # Đổi tên nếu khác
  if [[ "$file" != "$new_name" && -n "$new_name" ]]; then
    echo "🔄 Đổi tên: '$file' → '$new_name'"
    mv -i "$file" "$new_name"
  fi
done
