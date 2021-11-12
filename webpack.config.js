var path = require("path");

module.exports = {
  entry: path.join(__dirname, "srcjs", "selectControl.jsx"),
  output: {
    path: path.join(__dirname, "inst/www/shinySelect/selectControl"),
    filename: "selectControl.js"
  },
  module: {
    rules: [
      {
        test: /\.jsx?$/,
        loader: "babel-loader",
        options: {
          presets: ["@babel/preset-env", "@babel/preset-react"]
        }
      },
      // For CSS so that import "path/style.css"; works
      {
        test: /\.css$/,
        use: ["style-loader", "css-loader"]
      }
    ]
  },
  externals: {
    react: "window.React",
    "react-dom": "window.ReactDOM",
    reactR: "window.reactR"
  },
  stats: {
    colors: true
  },
  devtool: "source-map"
};
