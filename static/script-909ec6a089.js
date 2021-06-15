window.addEventListener('DOMContentLoaded', () => {
  const timestamp = document.getElementById('update-timestamp');
  if (timestamp != null) {
    if (timestamp.innerText != null) {
      const formatted = new Date(timestamp.innerText);
      timestamp.innerHTML = `${formatted.toLocaleDateString()}<br>${formatted.toLocaleTimeString()}`;
    }
  }
});
