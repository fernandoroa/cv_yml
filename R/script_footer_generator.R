rm(list = ls())
source("R/dictionaries.R")
source("R/produce_pdf.R")
source("R/footer_helper.R")

chunk_1 <- "
<script>
  document.addEventListener(\"DOMContentLoaded\", function () {
    var newNavbarHTML = `
      <div class=\"navbar navbar-default navbar-fixed-top\" role=\"navigation\">
        <div class=\"container\">
          <div class=\"navbar-header\">
            <button type=\"button\" class=\"navbar-toggle collapsed\" data-toggle=\"collapse\" data-target=\"#navbar\">
              <span class=\"icon-bar\"></span>
              <span class=\"icon-bar\"></span>
              <span class=\"icon-bar\"></span>
            </button>
            <a class=\"navbar-brand\" href=\"index.html\">CV Fernando Roa</a>
          </div>
          <div id=\"navbar\" class=\"navbar-collapse collapse\">
            <ul class=\"nav navbar-nav\">
              <li class=\"dropdown\">
                <a href=\"#\" class=\"dropdown-toggle\" data-toggle=\"dropdown\" role=\"button\" aria-haspopup=\"true\" aria-expanded=\"false\">Select Category <span class=\"caret\"></span></a>
                <ul class=\"dropdown-menu\" id=\"category-menu\">
"

chunk_3 <- "
                </ul>
              </li>
              <li class=\"dropdown\">
                <a href=\"#\" class=\"dropdown-toggle\" data-toggle=\"dropdown\" role=\"button\" aria-haspopup=\"true\" aria-expanded=\"false\">Select Language <span class=\"caret\"></span></a>
                <ul class=\"dropdown-menu\" id=\"language-menu\">
"

shared_params <- yaml::read_yaml("shared_params.yml")
config <- yaml::read_yaml("shared_params.yml")

profiles <- shared_params$valid_profiles
profiles <- profiles[profiles != "general"]
profiles_upper <- sub("([[:alpha:]])", "\\U\\1", profiles, perl = T)

languages <- shared_params$valid_languages
languages_upper <- sub("([[:alpha:]])", "\\U\\1", languages, perl = T)
languages_upper

language_self <- language_names[languages]

profile_accumulator <- character()
for (idx in seq_along(profiles_upper)) {
  html <- paste0("<li><a href=\"#\" onclick=\"setCategory('", profiles[idx], "'); return false;\">", profiles_upper[idx], "</a></li>")
  profile_accumulator <- c(profile_accumulator, html)
}

chunk_2 <- profile_accumulator

language_accumulator <- character()
for (idx in seq_along(languages_upper)) {
  html <- paste0(
    "<li><a href=\"#\" onclick=\"setLanguage('", languages_2codes[languages[idx]], "'); return false;\">",
    language_self[idx], "</a></li>"
  )
  language_accumulator <- c(language_accumulator, html)
}

chunk_4 <- language_accumulator

chunk_5 <- "
                </ul>
              </li>
            </ul>
          </div>
        </div>
      </div>
    `;

    var navbarFixedTop = document.querySelector(\".navbar-fixed-top\");
    if (navbarFixedTop) {
      navbarFixedTop.outerHTML = newNavbarHTML;
    }

    // Define the page mappings
"

combinations <- make_combi_df(shared_params)
combinations <- combinations |> arrange(language)
combinations$lang_2_letter <- languages_2codes[combinations$language]
combinations$profile_sec_vowel <- extract_after_second_vowel(combinations$profile)

combinations

output <- combinations %>%
  group_by(lang_2_letter) %>%
  summarise(
    developer = ifelse(any(profile_sec_vowel == "devel"), glue('developer: "index_{lang_2_letter}_devel.html"'), ""),
    academic = ifelse(any(profile_sec_vowel == "acad"), glue('academic: "index_{lang_2_letter}_acad.html"'), "")
  ) %>%
  mutate(
    text = glue("{lang_2_letter}: {{
      {developer},
      {academic},
    }},")
  ) %>%
  pull(text) %>%
  paste(collapse = "\n")

replacement <- "index.html"

result <- replace_first_html(output, replacement)

chunk_6 <- paste("const pages = {\n", result, "\n};", collapse = "\n")

language_codes <- languages_2codes[languages]

language_codes

output <- "const languageTitles = {\n"
for (i in seq_along(language_codes)) {
  output <- paste0(output, "  ", language_codes[i], ': "', language_self[i], '",\n')
}

chunk_7 <- paste0(output, "};\n")


# Generate the text
chunk_8 <- generateCategoryTitles(language_codes, academic[languages], developer[languages])

chunk_9 <- "
    // Function to handle language selection
    function setLanguage(language) {
      const currentCategory = getCategoryFromUrl(window.location.pathname);
      if (currentCategory) {
        const url = pages[language][currentCategory];
        window.location.href = url;
      }
    }

    // Function to handle category selection
    function setCategory(category) {
      const currentLanguage = getLanguageFromUrl(window.location.pathname);
      if (currentLanguage) {
        const url = pages[currentLanguage][category];
        window.location.href = url;
      }
    }

    // Get the current language from the URL
    function getLanguageFromUrl(url) {
      for (const language in pages) {
        for (const category in pages[language]) {
          if (
            url.endsWith(pages[language][category]) ||
            url.endsWith(\"curriculumpu\") ||
            url.endsWith(\"curriculumpu/\")
          ) {
            return language;
          }
        }
      }
      return \"en\"; // Default to English if not found
    }

    // Get the current category from the URL
    function getCategoryFromUrl(url) {
      for (const language in pages) {
        for (const category in pages[language]) {
          if (
            url.endsWith(pages[language][category]) ||
            url.endsWith(\"curriculumpu\") ||
            url.endsWith(\"curriculumpu/\")
          ) {
            return category;
          }
        }
      }
      return \"developer\"; // Default to developer if not found
    }

    // Set dropdown toggle text
    function setDropdownToggleText() {
      const currentPage = window.location.pathname;

      const language = getLanguageFromUrl(currentPage);
      const currentCategory = getCategoryFromUrl(currentPage);

      const languageDropdownToggle = document
        .querySelector(\"#language-menu\")
        .parentElement.querySelector(\".dropdown-toggle\");
      const categoryDropdownToggle = document
        .querySelector(\"#category-menu\")
        .parentElement.querySelector(\".dropdown-toggle\");

      if (languageDropdownToggle) {
        languageDropdownToggle.innerHTML = `${languageTitles[language]} <span class=\"caret\"></span>`;
      }

      if (categoryDropdownToggle) {
        categoryDropdownToggle.innerHTML = `${categoryTitles[language][currentCategory]} <span class=\"caret\"></span>`;
      }

      // Update menu items based on selected language
      const categoryMenu = document.getElementById(\"category-menu\");
      categoryMenu.innerHTML = `
"

profile_accumulator <- character()

for (idx in seq_along(profiles)) {
  html <- paste0("<li><a href=\"#\" onclick=\"setCategory('", profiles[idx], "'); return false;\">${categoryTitles[language].", profiles[idx], "}</a></li>")
  profile_accumulator <- c(profile_accumulator, html)
}

chunk_10 <- profile_accumulator
chunk_10

chunk_11 <- "
      `;

      const languageMenu = document.getElementById(\"language-menu\");
      languageMenu.innerHTML = `
"

language_accumulator <- character()
for (idx in seq_along(language_codes)) {
  html <- paste0("<li><a href=\"#\" onclick=\"setLanguage('", language_codes[idx], "'); return false;\">${languageTitles.", language_codes[idx], "}</a></li>")
  language_accumulator <- c(language_accumulator, html)
}

chunk_12 <- language_accumulator
chunk_12

chunk_13 <- "
`;
}

setDropdownToggleText();

// Attach click event listeners to the dropdown menu items
document.querySelectorAll(\"#language-menu a\").forEach((item) => {
  item.addEventListener(\"click\", function (event) {
    event.preventDefault();
    setLanguage(event.target.getAttribute(\"onclick\").split(\"'\")[1]);
  });
});

document.querySelectorAll(\"#category-menu a\").forEach((item) => {
  item.addEventListener(\"click\", function (event) {
    event.preventDefault();
    setCategory(event.target.getAttribute(\"onclick\").split(\"'\")[1]);
  });
});
});
</script>
"

chunks <- ls(pattern = "chun")

chunks <- order_by_numeric_part(chunks)

chunk_values <- mget(chunks)

collapsed_string <- sapply(chunk_values, function(x) paste(x, collapse = "\n"))

result <- paste(unlist(collapsed_string), collapse = "\n")

writeLines(result, "footer.html")

html_file <- "footer.html"

lint_html_js(html_file)
