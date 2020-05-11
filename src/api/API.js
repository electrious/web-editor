exports.getErrorMessage = err => {
    if (err.response && err.response.data) {
        const msg = err.response.data.message
        let str = ''
        Object.entries(msg).forEach(v => {
            str += v[0] + ': ' + v[1] + '\n'
        })

        return str
    } else {
        return err.message
    }
}
