/// helper functions: don't edit this file.

/* Simple hash function from https://gist.github.com/iperelivskiy/4110988
   Not meant to be particularly secure, just easier for you to solve the problem than reverse the hash
   */
function hash(s) {
    let a = 1, c = 0, h, o;
    if (s) {
        a = 0;
        for (h = s.length - 1; h >= 0; h--) {
            o = s.charCodeAt(h);
            a = (a<<6&268435455) + o + (o<<14);
            c = a & 266338304;
            a = c!==0?a^c>>21:a;
        }
    }
    return String(a);
};
