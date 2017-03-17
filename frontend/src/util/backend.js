import request from 'request'

const base_url = 'http://' + window.location.hostname + ':5000'

const authenticate = (sessionId) => ({
  get: (path) => {
    return request
      .get({
        url: base_url + path,
        'servant-session-id': sessionId
      })
  },

  post: (path, payload) => {
    return request
      .post({
        url: base_url + path,
        'servant-session-id': sessionId
      })
      .json(payload)
  }
})

const post = (path, payload) => {
  return request
    .post(base_url + path)
    .json(payload)
}

export default {
  base_url,
  authenticate,
  post
}


