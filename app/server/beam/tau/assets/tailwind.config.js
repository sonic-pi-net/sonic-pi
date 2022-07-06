const colors = require("tailwindcss/colors");

module.exports = {
  content: [
    "../lib/*_web/**/*.*ex",
    "./js/**/*.js",
    (process.env.MIX_DEPS_PATH || "../deps") + "/petal_components/**/*.*ex",
  ],
  theme: {
    extend: {
      colors: {
        primary: colors.pink,
        secondary: colors.blue,
      },
    },
  },
  plugins: [require("@tailwindcss/forms")],
  darkMode: "class",
};
