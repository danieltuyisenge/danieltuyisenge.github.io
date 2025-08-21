<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>{{ page.title | default: "Daniel Tuyisenge" }}</title>
  <link rel="stylesheet" href="{{ "/style.css" | relative_url }}">
  <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css" rel="stylesheet">
  <style>
    /* HERO STYLES */
    .hero-section {
      position: relative;
      height: 100vh;
      background: url('/images/profile.jpg') no-repeat center center/cover;
      color: white;
      display: flex;
      flex-direction: column;
      justify-content: flex-end;
    }
    .hero-overlay {
      position: absolute;
      top: 0; left: 0;
      width: 100%; height: 100%;
      background: rgba(0,0,0,0.5);
    }
    .hero-content {
      position: relative;
      z-index: 2;
      padding: 3rem;
    }
    .hero-title {
      font-size: 3rem;
      font-weight: bold;
    }
    .hero-subtitle {
      font-size: 1.5rem;
    }
    .contact-box {
      position: relative;
      z-index: 2;
      background: rgba(0,0,0,0.7);
      padding: 1rem 1.5rem;
      border-radius: 8px;
      margin-top: 2rem;
      display: inline-block;
    }
  </style>
</head>
<body>

<!-- NAVBAR -->
<nav class="navbar navbar-expand-lg navbar-dark bg-dark fixed-top">
  <div class="container-fluid">
    <a class="navbar-brand" href="{{ "/" | relative_url }}">Daniel Tuyisenge</a>
    <button class="navbar-toggler" type="button" data-bs-toggle="collapse" data-bs-target="#navbarNav">
      <span class="navbar-toggler-icon"></span>
    </button>
    <div class="collapse navbar-collapse" id="navbarNav">
      <ul class="navbar-nav ms-auto">
        <li class="nav-item"><a class="nav-link" href="{{ "/" | relative_url }}">Home</a></li>
        <li class="nav-item"><a class="nav-link" href="{{ "/cv" | relative_url }}">CV</a></li>
        <li class="nav-item"><a class="nav-link" href="{{ "/research" | relative_url }}">Research</a></li>
        <li class="nav-item"><a class="nav-link" href="{{ "/code" | relative_url }}">Code</a></li>
        <li class="nav-item"><a class="nav-link" href="{{ "/contact" | relative_url }}">Contact</a></li>
      </ul>
    </div>
  </div>
</nav>

<!-- HERO SECTION -->
<header class="hero-section">
  <div class="hero-overlay"></div>
  <div class="hero-content">
    <h1 class="hero-title">Daniel Tuyisenge</h1>
    <h3 class="hero-subtitle">Ph.D. Candidate in Statistics<br>University of Kentucky</h3>

    <div class="contact-box">
      <p>Email: <a href="mailto:daniel.tuyisenge@uky.edu" style="color:#FFD700;">daniel.tuyisenge@uky.edu</a></p>
      <p>Research: Tolerance Intervals | Randomized Response | Outlier Detection</p>
    </div>
  </div>
</header>

<!-- MAIN CONTENT -->
<main class="container my-5">
  {{ content }}
</main>

<!-- FOOTER -->
<footer class="bg-dark text-white text-center p-3 mt-5">
  © {{ "now" | date: "%Y" }} Daniel Tuyisenge | Built with GitHub Pages
</footer>

<script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/js/bootstrap.bundle.min.js"></script>
</body>
</html>
