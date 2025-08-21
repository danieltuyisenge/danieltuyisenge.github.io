<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>{{ page.title }}</title>
  <link rel="stylesheet" href="style.css">
  <style>
    nav {
      text-align: center;
      background: #f8f9fa;
      padding: 10px;
      border-bottom: 1px solid #ddd;
      font-family: Arial, sans-serif;
    }
    nav a {
      margin: 0 15px;
      text-decoration: none;
      font-weight: bold;
      color: #004080;
    }
    nav a:hover {
      text-decoration: underline;
    }
    main {
      padding: 20px;
      max-width: 900px;
      margin: auto;
    }
  </style>
</head>
<body>
  <nav>
    <a href="index.html">Home</a>
    <a href="cv.html">CV</a>
    <a href="research.html">Research</a>
    <a href="code.html">Code</a>
    <a href="contact.html">Contact</a>
  </nav>

  <main>
    {{ content }}
  </main>
</body>
</html>

