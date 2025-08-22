const canvas = document.getElementById("bg");

if (canvas) {
  const ctx = canvas.getContext("2d");

  // set size
  function resizeCanvas() {
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
  }
  resizeCanvas();

  // generate stars
  const stars = [];
  const numStars = 150;

  function createStars() {
    stars.length = 0; // reset stars on resize
    for (let i = 0; i < numStars; i++) {
      stars.push({
        x: Math.random() * canvas.width,
        y: Math.random() * canvas.height,
        radius: Math.random() * 2 + 0.5,
        dx: (Math.random() - 0.5) * 0.6,
        dy: (Math.random() - 0.5) * 0.6
      });
    }
  }

  createStars();

  // animate stars
  function animate() {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    ctx.fillStyle = "white";

    stars.forEach(star => {
      ctx.beginPath();
      ctx.arc(star.x, star.y, star.radius, 0, Math.PI * 2);
      ctx.fill();

      star.x += star.dx;
      star.y += star.dy;

      // bounce off edges
      if (star.x < 0 || star.x > canvas.width) star.dx *= -1;
      if (star.y < 0 || star.y > canvas.height) star.dy *= -1;
    });

    requestAnimationFrame(animate);
  }

  animate();

  // handle window resize
  window.addEventListener("resize", () => {
    resizeCanvas();
    createStars();
  });
}


