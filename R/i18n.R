# ==============================================================================
# CCY Humanitarian Deduplication Platform · Internationalization (i18n) Module
# Humanitarian terminology standard for CCY Yemen and UN Cash Working Group
# ==============================================================================

I18N_DICT <- list(
  # Topbar & Branding
  "app_name" = list(
    en = "CCY Deduplication Platform",
    ar = "منصة مطابقة وتدقيق البيانات - CCY"
  ),
  "db_synced" = list(
    en = "🟢 Master DB Synced: %s (Offline Ready)",
    ar = "🟢 قاعدة البيانات المركزية مُحدثة: %s (جاهز للعمل)"
  ),
  "db_needed" = list(
    en = "🟡 Master DB: Snapshot Needed",
    ar = "🟡 قاعدة البيانات المركزية: يلزم تنزيل نسخة"
  ),
  "time_just_now" = list(
    en = "Just now",
    ar = "الآن"
  ),
  "time_mins_ago" = list(
    en = "%d m ago",
    ar = "منذ %d دقيقة"
  ),
  "time_hours_ago" = list(
    en = "%.1f h ago",
    ar = "منذ %.1f ساعة"
  ),
  "time_days_ago" = list(
    en = "%.1f d ago",
    ar = "منذ %.1f يوم"
  ),
  "btn_back_to_workflow" = list(
    en = "Back to workflow",
    ar = "العودة إلى مسار العمل"
  ),
  "btn_settings" = list(
    en = "Settings",
    ar = "الإعدادات"
  ),
  "btn_admin" = list(
    en = "Admin",
    ar = "لوحة الإدارة"
  ),
  "btn_users" = list(
    en = "Users",
    ar = "المستخدمون"
  ),
  "btn_logout" = list(
    en = "Log out",
    ar = "تسجيل الخروج"
  ),
  "btn_login" = list(
    en = "Log in",
    ar = "تسجيل الدخول"
  ),
  "btn_cancel" = list(
    en = "Cancel",
    ar = "إلغاء"
  ),
  "login_email" = list(
    en = "Email",
    ar = "البريد الإلكتروني"
  ),
  "login_password" = list(
    en = "Password",
    ar = "كلمة المرور"
  ),

  # Stepper Navigation
  "step_upload_title" = list(
    en = "Upload & Verify",
    ar = "الرفع والتحقق"
  ),
  "step_upload_desc" = list(
    en = "Data Hygiene",
    ar = "سلامة البيانات"
  ),
  "step_mapping_title" = list(
    en = "Map Columns",
    ar = "مطابقة الحقول"
  ),
  "step_mapping_desc" = list(
    en = "Field Matching",
    ar = "تحديد الأعمدة"
  ),
  "step_strategy_title" = list(
    en = "Configure",
    ar = "الإعدادات والمعايير"
  ),
  "step_strategy_desc" = list(
    en = "Rules & Thresholds",
    ar = "القواعد والنسب"
  ),
  "step_matching_title" = list(
    en = "Run Matching",
    ar = "تشغيل المطابقة"
  ),
  "step_matching_desc" = list(
    en = "Pairwise Engine",
    ar = "محرك الفحص"
  ),
  "step_results_title" = list(
    en = "Results",
    ar = "النتائج والتصدير"
  ),
  "step_results_desc" = list(
    en = "Review & Export",
    ar = "المراجعة والتنزيل"
  ),
  "badge_current" = list(
    en = "Current",
    ar = "الحالي"
  ),
  "badge_done" = list(
    en = "Done",
    ar = "مكتمل"
  ),
  "badge_pending" = list(
    en = "Pending",
    ar = "قيد الانتظار"
  ),
  "system_settings" = list(
    en = "System Settings",
    ar = "إعدادات النظام"
  ),
  "settings_area" = list(
    en = "Settings Area",
    ar = "منطقة الإعدادات"
  ),

  # Step 1: Upload & Fetch
  "card_upload_title" = list(
    en = "Upload & Fetch Data",
    ar = "رفع وجلب البيانات"
  ),
  "label_upload_file" = list(
    en = "Upload spreadsheet",
    ar = "رفع جدول البيانات"
  ),
  "link_download_template" = list(
    en = "Download template",
    ar = "تحميل القالب القياسي"
  ),
  "hint_accepted_files" = list(
    en = "Accepted file types: .xlsx, .xls, .csv — Max size: 10 MB.",
    ar = "الملفات المقبولة: .xlsx, .xls, .csv — الحد الأقصى للحجم: 10 ميجابايت."
  ),
  "btn_fetch_master" = list(
    en = "Fetch master database",
    ar = "جلب قاعدة البيانات المركزية"
  ),
  "btn_cancel_fetch" = list(
    en = "Cancel fetch",
    ar = "إلغاء عملية الجلب"
  ),
  "btn_confirm_upload" = list(
    en = "Confirm upload & continue",
    ar = "تأكيد الرفع والمتابعة"
  ),
  "card_health_title" = list(
    en = "Data Health & Verification",
    ar = "فحص جودة البيانات والتحقق"
  ),
  "no_file_uploaded_title" = list(
    en = "No spreadsheet uploaded yet",
    ar = "لم يتم رفع جدول بيانات بعد"
  ),
  "no_file_uploaded_desc" = list(
    en = "Upload an Excel (.xlsx/.xls) or CSV partner list on the left to verify record health, run automated hygiene audits, and preview rows.",
    ar = "قم برفع قائمة الشريك بتنسيق إكسل أو CSV من الجهة المقابلة للتحقق من سلامة السجلات وإجراء تدقيق الجودة الآلي ومعاينة الصفوف."
  ),
  "feature_instant_health" = list(
    en = "Coverage analysis for IDs & phone numbers",
    ar = "تحليل فوري لنسبة اكتمال أرقام الهويات والهواتف"
  ),
  "feature_data_hygiene" = list(
    en = "Automatic scans for scientific notation (e.g. 7.71E+08), empty rows, duplicate headers, and placeholder sequences",
    ar = "فحص تلقائي للصيغ العلمية والصفوف الفارغة وتكرار الترويسات والرموز الوهمية"
  ),
  "feature_data_protection" = list(
    en = "Zero external transmission; processed in-memory",
    ar = "معالجة فورية آمنة دون تخزين خارجي للبيانات الحساسة"
  ),
  "file_verified" = list(
    en = "✓ File Verified",
    ar = "✓ تم التحقق من الملف"
  ),
  "kpi_total_records" = list(
    en = "Total Records",
    ar = "إجمالي السجلات"
  ),
  "kpi_id_coverage" = list(
    en = "National ID Coverage",
    ar = "تغطية أرقام الهويات"
  ),
  "kpi_phone_coverage" = list(
    en = "Phone Coverage",
    ar = "تغطية أرقام الهواتف"
  ),
  "kpi_dup_ids" = list(
    en = "Raw Duplicate IDs",
    ar = "تكرار أرقام الهوية المباشر"
  ),
  "unmapped" = list(
    en = "Unmapped",
    ar = "غير محدد"
  ),
  "clean_zero" = list(
    en = "0 (Clean)",
    ar = "0 (سليم)"
  ),
  "found_count" = list(
    en = "%d Found",
    ar = "%d تكرار"
  ),
  "hygiene_box_title" = list(
    en = "Pre-Upload Data Hygiene & Quality Audits",
    ar = "فحوصات سلامة وجودة البيانات قبل الرفع"
  ),
  "hygiene_all_passed" = list(
    en = "✓ All 4 Hygiene Audits Passed",
    ar = "✓ اجتازت جميع فحوصات الجودة الـ 4"
  ),
  "hygiene_warnings_count" = list(
    en = "⚠️ %d Quality Notice(s)",
    ar = "⚠️ %d ملاحظة جودة"
  ),
  "audit_sci_title" = list(
    en = "Scientific Format",
    ar = "الصيغة العلمية للأرقام"
  ),
  "audit_sci_clean" = list(
    en = "✓ Clean",
    ar = "✓ سليم"
  ),
  "audit_sci_label" = list(
    en = "No exponential numbers",
    ar = "خالٍ من الأرقام الأسية"
  ),
  "audit_sci_detail" = list(
    en = "Guarantees 9-digit phones and 11-digit IDs are intact.",
    ar = "يضمن سلامة أرقام الهواتف (9 أرقام) وأرقام الهويات (11 رقماً)."
  ),
  "audit_empty_title" = list(
    en = "Empty Row Audit",
    ar = "فحص الصفوف الفارغة"
  ),
  "audit_empty_clean" = list(
    en = "✓ 0 Blank",
    ar = "✓ خالٍ من الفراغات"
  ),
  "audit_empty_label" = list(
    en = "Blank rows auto-pruned",
    ar = "استبعاد الصفوف الفارغة تلقائياً"
  ),
  "audit_empty_detail" = list(
    en = "Empty rows pruned to prevent indexing offset.",
    ar = "تم استبعاد الصفوف الفارغة لمنع حدوث إزاحة في الترقيم."
  ),
  "audit_header_title" = list(
    en = "Header Integrity",
    ar = "سلامة ترويسة الأعمدة"
  ),
  "audit_header_clean" = list(
    en = "✓ All Unique",
    ar = "✓ جميع الأعمدة فريدة"
  ),
  "audit_header_label" = list(
    en = "Unique column headers",
    ar = "عناوين أعمدة فريدة وغير مكررة"
  ),
  "audit_header_detail" = list(
    en = "Prevents column collisions during mapping.",
    ar = "يمنع تعارض الحقول أثناء عملية المطابقة."
  ),
  "audit_dummy_title" = list(
    en = "Placeholder Scan",
    ar = "فحص القيم الوهمية"
  ),
  "audit_dummy_clean" = list(
    en = "✓ Clean",
    ar = "✓ سليم"
  ),
  "audit_dummy_label" = list(
    en = "Dummy placeholder filter",
    ar = "تصفية القيم والرموز التجريبية"
  ),
  "audit_dummy_detail" = list(
    en = "Excludes generic values (e.g. 0000) from exact matching.",
    ar = "استبعاد القيم العامة (مثل 0000 أو NA) من المطابقة التامة."
  ),
  "tab_preview" = list(
    en = "📄 Upload Preview",
    ar = "📄 معاينة البيانات المرفوعة"
  ),
  "tab_inversion_audit" = list(
    en = "🔍 Duplication & Inversion Audit",
    ar = "🔍 تدقيق التكرارات والنزوح المعكوس"
  ),

  # Step 2: Mapping
  "card_mapping_title" = list(
    en = "Confirm Fields Mapping",
    ar = "تأكيد مطابقة الحقول"
  ),
  "label_match_fields" = list(
    en = "Fields to use for matching:",
    ar = "الحقول المستخدمة في خوارزمية المطابقة:"
  ),
  "field_partner" = list(
    en = "Partner",
    ar = "اسم المنظمة الشريكة (Partner)"
  ),
  "field_id" = list(
    en = "ID number",
    ar = "رقم الهوية الوطنية (National ID)"
  ),
  "field_phone" = list(
    en = "Phone number",
    ar = "رقم الهاتف (Phone Number)"
  ),
  "field_hoh_name" = list(
    en = "Head of household name",
    ar = "اسم رب الأسرة (HoH Name)"
  ),
  "field_spouse_name" = list(
    en = "Spouse name",
    ar = "اسم الزوج / الزوجة (Spouse Name)"
  ),
  "field_geography" = list(
    en = "Geography (governorate/district/subdistrict/village)",
    ar = "الموقع الجغرافي (المحافظة/المديرية/العزلة/القرية)"
  ),
  "btn_save_preset" = list(
    en = "💾 Save as Preset",
    ar = "💾 حفظ كقالب مخصص"
  ),
  "btn_confirm_mapping" = list(
    en = "Confirm mapping",
    ar = "تأكيد مطابقة الحقول والمتابعة"
  ),

  # Step 3: Strategy & Rules
  "card_strategy_title" = list(
    en = "Configure Matching Parameters",
    ar = "ضبط معايير وخوارزمية المطابقة"
  ),
  "slider_threshold_high" = list(
    en = "High confidence threshold:",
    ar = "حد التطابق عالي الثقة:"
  ),
  "helper_threshold_high" = list(
    en = "Pairs scoring at or above this threshold are classified as high-confidence matches.",
    ar = "تعتبر الحالات التي تحقق هذه النسبة أو أعلى مطابقات مؤكدة."
  ),
  "slider_threshold_medium" = list(
    en = "Medium confidence threshold:",
    ar = "حد التطابق متوسط الثقة:"
  ),
  "helper_threshold_medium" = list(
    en = "Pairs scoring between medium and high thresholds are flagged for manual review.",
    ar = "تعتبر الحالات بين الحدين المتوسط والعالي حالات محتملة تتطلب مراجعة مكتبية أو ميدانية."
  ),
  "slider_max_candidates" = list(
    en = "Max candidate pairs:",
    ar = "الحد الأقصى لأزواج المقارنة المرشحة:"
  ),
  "helper_max_candidates" = list(
    en = "Caps the number of candidate comparisons evaluated per block (maximum 2,000 pairs).",
    ar = "يحدد الحد الأقصى للمقارنات التوافقية لكل كتلة بيانات (الحد الأقصى 2,000 زوج)."
  ),
  "mpca_filter_title" = list(
    en = "📅 MPCA Last Distribution Date Filter (تصفية تاريخ آخر توزيع)",
    ar = "📅 تصفية تاريخ آخر استلام للمساعدات النقدية MPCA"
  ),
  "mpca_filter_checkbox" = list(
    en = "Deduplicate only against beneficiaries with MPCA distribution in < 6 months",
    ar = "مطابقة وتدقيق فقط مع المستفيدين الذين استلموا مساعدات MPCA خلال أقل من 6 أشهر"
  ),
  "mpca_filter_desc" = list(
    en = "When checked, the engine filters the central master database to only match against beneficiaries whose last MPCA distribution date (Dist_Date_Calc_New) was received within the last 6 months (180 days). Beneficiaries assisted earlier are excluded from the check.",
    ar = "عند التفعيل، يقوم النظام بتصفية قاعدة البيانات المركزية ومطابقة الحالات فقط مع المستفيدين الذين كان تاريخ استلامهم لآخر مساعدة نقدية (Last Receipt Date) خلال الـ 6 أشهر الماضية (180 يوماً). ويتم استبعاد المستفيدين الذين تلقوا المساعدة قبل ذلك لإتاحة إعادة استحقاقهم وفق معايير الكتلة."
  ),
  "slider_mpca_window" = list(
    en = "Assistance Recency Window (Months / نافذة الأشهر):",
    ar = "نافذة حداثة المساعدة النقدية (بالأشهر):"
  ),
  "btn_confirm_strategy" = list(
    en = "Continue to matching",
    ar = "المتابعة إلى مرحلة المطابقة"
  ),
  "card_guide_title" = list(
    en = "Strategy & Deduplication Guide",
    ar = "دليل استراتيجية المطابقة وتوحيد البيانات"
  ),
  "guide_sops_title" = list(
    en = "CCY Consortium Matching SOPs:",
    ar = "إجراءات التشغيل القياسية لكتلة المساعدات النقدية (CCY SOPs):"
  ),
  "guide_sops_desc" = list(
    en = "Standard deduplication combines exact national ID/phone checks with weighted 4-part Arabic name decomposition (Jaro-Winkler + Levenshtein).",
    ar = "تجمع المطابقة القياسية بين الفحص التام لأرقام الهويات الوطنية والهواتف، والتحليل الرباعي الموزون للأسماء العربية (Jaro-Winkler + Levenshtein)."
  ),
  "guide_high_label" = list(
    en = "High Confidence (≥90%): ",
    ar = "تطابق عالي الثقة (≥90%): "
  ),
  "guide_high_desc" = list(
    en = "Confirmed duplicates requiring immediate action or partner reconciliation.",
    ar = "حالات تطابق مؤكدة تتطلب استبعاداً فورياً أو تسوية وتنسيقاً بين الشركاء."
  ),
  "guide_medium_label" = list(
    en = "Medium Review (75%–89%): ",
    ar = "مراجعة متوسطة الثقة (75%–89%): "
  ),
  "guide_medium_desc" = list(
    en = "Probable matches queued for field verification.",
    ar = "حالات تطابق محتملة تُدرج في قائمة التحقق الميداني والمكتبي."
  ),
  "guide_mpca_label" = list(
    en = "MPCA Date Filter: ",
    ar = "تصفية تاريخ المساعدة MPCA: "
  ),
  "guide_mpca_desc" = list(
    en = "Allows targeting beneficiaries with recent assistance (<6 months) to avoid re-assisting within the active MPCA cycle while enabling re-eligibility after 6 months.",
    ar = "يتيح استهداف المستفيدين الذين تلقوا مساعدة نقدية مؤخراً (<6 أشهر) لمنع الازدواجية خلال دورة المساعدة النشطة، مع السماح بإعادة تسجيلهم بعد انقضاء المدة."
  ),

  # Step 4: Run Matching
  "card_matching_title" = list(
    en = "Run Matching",
    ar = "بدء تشغيل عملية المطابقة"
  ),
  "matching_when_run" = list(
    en = "When you run matching:",
    ar = "عند بدء تشغيل عملية المطابقة:"
  ),
  "matching_bullet_1" = list(
    en = "The app will load the latest master snapshot and preprocess the uploaded file.",
    ar = "يقوم النظام بتحميل أحدث نسخة من قاعدة البيانات المركزية ومعالجة ملف الشريك المرفوع مسبقاً."
  ),
  "matching_bullet_2" = list(
    en = "Candidate pairs will be generated using the selected match fields (capped at the configured max candidates).",
    ar = "يتم توليد أزواج المقارنة المرشحة بالاعتماد على الحقول المختارة وتوزيع الكتل الجغرافية."
  ),
  "matching_bullet_3" = list(
    en = "Each candidate pair is scored and classified into High / Medium confidence.",
    ar = "يتم احتساب درجة التشابه لكل زوج وتصنيفه تلقائياً إلى تطابق عالي الثقة أو مراجعة متوسطة."
  ),
  "matching_bullet_4" = list(
    en = "A running job can be stopped using \"Stop & start over\"; stopping will cancel the job and clear uploaded data.",
    ar = "يمكن إيقاف العملية الجارية عبر زر \"إيقاف وإعادة البدء\" في حال الرغبة بإلغاء المعالجة."
  ),
  "matching_bullet_5" = list(
    en = "Results are saved and an export button will be enabled when matching completes.",
    ar = "تُحفظ النتائج فور الاكتمال ويتم تفعيل زر تصدير ملف الإكسل الشامل."
  ),
  "btn_run_matching" = list(
    en = "Run matching",
    ar = "تشغيل المطابقة"
  ),
  "btn_rerun_matching" = list(
    en = "Re-run matching",
    ar = "إعادة تشغيل المطابقة"
  ),
  "btn_matching_progress" = list(
    en = "Matching in progress...",
    ar = "المطابقة قيد التنفيذ..."
  ),
  "btn_stop_job" = list(
    en = "Stop & start over",
    ar = "إيقاف وإعادة البدء"
  ),

  # Step 5: Results & Export
  "card_results_dossier" = list(
    en = "Deduplication Results Dossier",
    ar = "ملف تقرير نتائج مطابقة البيانات"
  ),
  "card_export_actions" = list(
    en = "Export & Actions",
    ar = "تصدير النتائج والإجراءات"
  ),
  "btn_export_results" = list(
    en = "Export results (Excel)",
    ar = "تصدير النتائج (Excel)"
  ),
  "btn_restart_dedup" = list(
    en = "🔄 Start New Deduplication Run",
    ar = "🔄 بدء عملية مطابقة جديدة"
  ),
  "export_will_enable" = list(
    en = "Export will be enabled once matching completes.",
    ar = "سيتوفر التصدير فور اكتمال عملية المطابقة."
  ),
  "total_identified" = list(
    en = "Total Identified:",
    ar = "إجمالي الحالات المكتشفة:"
  ),
  "potential_duplicates" = list(
    en = "potential duplicates",
    ar = "حالة تكرار محتملة"
  ),
  "export_dossier_desc" = list(
    en = "The exported Excel dossier contains all executive summary metrics, high confidence pairs, medium review queue, internal duplicates, and run metadata.",
    ar = "يتضمن ملف التقرير المُصدَّر كافة مؤشرات الملخص التنفيذي، وقوائم التطابق العالي، وقائمة المراجعة المتوسطة، والتكرارات الداخلية، وبيانات الاعتماد."
  ),

  # Settings
  "settings_header" = list(
    en = "Settings",
    ar = "إعدادات النظام والاتصال"
  ),
  "label_signed_in_email" = list(
    en = "Signed-in email",
    ar = "البريد الإلكتروني المسجل"
  ),
  "label_ai_token" = list(
    en = "ActivityInfo token",
    ar = "رمز تفويض ActivityInfo (Token)"
  ),
  "label_ai_form_id" = list(
    en = "ActivityInfo table (form) ID",
    ar = "معرف جدول أو استمارة ActivityInfo (Form ID)"
  ),
  "btn_save_settings" = list(
    en = "Save settings",
    ar = "حفظ الإعدادات"
  ),
  "btn_manage_mfa" = list(
    en = "Manage MFA",
    ar = "إدارة التحقق بخطوتين (MFA)"
  ),

  # Admin
  "admin_header_title" = list(
    en = "System Administration & Control Center",
    ar = "لوحة إدارة النظام ومركز التحكم"
  ),
  "admin_header_desc" = list(
    en = "Manage authorized user accounts, partner directories, compliance audit trails, and system backups.",
    ar = "إدارة حسابات المستخدمين المصرح لهم، دليل الشركاء، سجلات التدقيق والامتثال، والنسخ الاحتياطية."
  ),
  "admin_tab_users" = list(
    en = "👥 User Management",
    ar = "👥 إدارة المستخدمين"
  ),
  "admin_tab_partners" = list(
    en = "🏢 Partner Directory",
    ar = "🏢 دليل الشركاء والمنظمات"
  ),
  "admin_tab_audit" = list(
    en = "📜 Compliance & Audit Logs",
    ar = "📜 سجلات التدقيق والامتثال"
  ),
  "admin_tab_backups" = list(
    en = "💾 System Backups & Snapshots",
    ar = "💾 النسخ الاحتياطية واللقطات"
  )
)

#' Translate a key into the active language
#'
#' @param key Character key identifying the UI text string.
#' @param lang Language code ("en" or "ar"). Defaults to "en".
#' @param ... Optional arguments passed to sprintf if the format string contains % placeholders.
#' @return Translated character string.
tr <- function(key, lang = "en", ...) {
  if (is.null(key) || !is.character(key) || length(key) == 0) return("")
  if (length(key) > 1) {
    return(vapply(key, function(k) tr(k, lang = lang, ...), character(1), USE.NAMES = FALSE))
  }
  
  entry <- I18N_DICT[[key]]
  if (is.null(entry)) {
    val <- key
  } else {
    lang_key <- if (identical(lang, "ar")) "ar" else "en"
    val <- entry[[lang_key]]
    if (is.null(val)) {
      val <- entry[["en"]]
    }
    if (is.null(val)) {
      val <- key
    }
  }

  dots <- list(...)
  if (length(dots) > 0) {
    tryCatch({
      val <- do.call(sprintf, c(list(val), dots))
    }, error = function(e) val)
  }

  val
}

#' Format a relative timestamp into localized text
#'
#' @param mtime POSIXct or POSIXlt timestamp.
#' @param lang Language code ("en" or "ar"). Defaults to "en".
#' @return Character string formatted relative to current time.
format_relative_time_i18n <- function(mtime, lang = "en") {
  if (is.null(mtime) || is.na(mtime)) return("")
  diff_hrs <- as.numeric(difftime(Sys.time(), mtime, units = "hours"))
  if (is.na(diff_hrs) || diff_hrs < 0) diff_hrs <- 0
  
  if (diff_hrs < 0.1) {
    tr("time_just_now", lang = lang)
  } else if (diff_hrs < 1) {
    mins <- as.integer(round(diff_hrs * 60))
    tr("time_mins_ago", lang = lang, mins)
  } else if (diff_hrs < 24) {
    tr("time_hours_ago", lang = lang, round(diff_hrs, 1))
  } else {
    days <- round(diff_hrs / 24, 1)
    tr("time_days_ago", lang = lang, days)
  }
}
