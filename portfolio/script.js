// Smooth page fade-in/out transitions
document.addEventListener("DOMContentLoaded", () => {
  const links = document.querySelectorAll("nav a");

  links.forEach(link => {
    link.addEventListener("click", e => {
      e.preventDefault();
      const target = link.getAttribute("href");

      document.body.style.transition = "opacity 0.7s";
      document.body.style.opacity = 0;

      setTimeout(() => {
        window.location.href = target;
      }, 700);
    });
  });

  // Fade back in after navigation
  document.body.style.opacity = 0;
  setTimeout(() => {
    document.body.style.transition = "opacity 1s";
    document.body.style.opacity = 1;
  }, 100);
});
