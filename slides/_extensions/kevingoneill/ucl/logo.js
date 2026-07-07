// Adapted from: https://github.com/orgs/quarto-dev/discussions/5439#discussioncomment-5828141
function update_logo() {
    let logo = document.querySelector(".slide-logo");
    
    if (logo.parentElement.classList.contains('has-dark-background')) {
        logo.src = "https://github.com/kevingoneill/ucl-revealjs/raw/refs/heads/main/_extensions/ucl/logo-light.svg";
    } else {
        logo.src = "https://github.com/kevingoneill/ucl-revealjs/raw/refs/heads/main/_extensions/ucl/logo-dark.svg";
    }
}


Reveal.on('ready', (event) => {
    setTimeout(update_logo, 10);
});

Reveal.on('slidechanged', (event) => {
    setTimeout(update_logo, 10);
});
