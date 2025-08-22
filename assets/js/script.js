// ============================
// Portfolio Website Script
// ============================

// 1) Toggle Sidebar (for mobile view)
const menuToggle = document.querySelector('.menu-toggle');
const sidebar = document.querySelector('.sidebar');

if (menuToggle) {
  menuToggle.addEventListener('click', () => {
    sidebar.classList.toggle('active');
  });
}

// Optional: Close sidebar when a link is clicked (mobile)
document.querySelectorAll('.sidebar a').forEach(link => {
  link.addEventListener('click', () => {
    if (window.innerWidth <= 768) {
      sidebar.classList.remove('active');
    }
  });
});

// 2) Smooth Scrolling for internal links
document.querySelectorAll('a[href^="#"]').forEach(anchor => {
  anchor.addEventListener("click", function(e) {
    e.preventDefault();
    document.querySelector(this.getAttribute("href")).scrollIntoView({
      behavior: "smooth"
    });
  });
});

// 3) Animate skill bars on load
window.addEventListener("load", () => {
  document.querySelectorAll(".bar span").forEach(bar => {
    let width = bar.style.width; // initial value from HTML
    bar.style.width = "0"; // reset to 0 for animation
    setTimeout(() => {
      bar.style.width = width;
    }, 200);
  });
});

// 4) Dark/Light Theme Toggle with Icon Swap
const themeToggle = document.querySelector('.theme-toggle');
const body = document.body;

// Load saved theme from localStorage
if (localStorage.getItem("theme") === "light") {
  body.classList.add("light-theme");
  if (themeToggle) themeToggle.textContent = "☀️"; // sun for light
} else {
  if (themeToggle) themeToggle.textContent = "🌙"; // moon for dark
}

// Toggle theme when clicked
if (themeToggle) {
  themeToggle.addEventListener('click', () => {
    body.classList.toggle("light-theme");

    if (body.classList.contains("light-theme")) {
      localStorage.setItem("theme", "light");
      themeToggle.textContent = "☀️";
    } else {
      localStorage.setItem("theme", "dark");
      themeToggle.textContent = "🌙";
    }
  });
}

