// LibreWolf user.js
// Override values from prefs.js.

// --- Enable userChrome.css / userContent.css ---
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);

// --- UI ---
user_pref("browser.compactmode.show", true);
user_pref("browser.uidensity", 1);
user_pref("browser.theme.toolbar-theme", 0);
user_pref("browser.toolbars.bookmarks.visibility", "never");
user_pref("browser.urlbar.showSearchSuggestionsFirst", false);
user_pref("sidebar.visibility", "hide-on-close");
user_pref("browser.display.document_color_use", 0);
user_pref("font.name.serif.x-cyrillic", "Noto Sans Mono CJK HK");

// --- New Tab ---
user_pref("browser.newtabpage.activity-stream.hideLogo", true);
user_pref("browser.newtabpage.activity-stream.topSitesRows", 2);
user_pref("browser.newtabpage.activity-stream.feeds.topsites", true);
user_pref("browser.newtabpage.activity-stream.widgets.enabled", true);

// --- Behavior ---
user_pref("browser.startup.page", 3);
user_pref("browser.download.always_ask_before_handling_new_types", true);
user_pref("browser.translations.neverTranslateLanguages", "ru");
user_pref("dom.forms.autocomplete.formautofill", true);
user_pref("devtools.chrome.enabled", true);
user_pref("browser.nova.enabled", false);

// --- Languages ---
user_pref("intl.accept_languages", "ru,en-us,en");
user_pref("intl.regional_prefs.use_os_locales", true);
user_pref("privacy.spoof_english", 2);

// --- Privacy ---
user_pref("browser.contentblocking.category", "strict");
user_pref("privacy.trackingprotection.enabled", true);
user_pref("privacy.trackingprotection.socialtracking.enabled", true);
user_pref("privacy.trackingprotection.emailtracking.enabled", true);
user_pref("privacy.fingerprintingProtection", true);
user_pref("privacy.bounceTrackingProtection.mode", 1);
user_pref("privacy.query_stripping.enabled", true);
user_pref("privacy.query_stripping.enabled.pbmode", true);
user_pref("privacy.history.custom", true);
user_pref("privacy.sanitize.sanitizeOnShutdown", false);
