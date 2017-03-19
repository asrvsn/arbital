import request from 'request'

const base_url = 'http://' + window.location.hostname + ':5000'

const authenticate = (sessionId) => ({
  get: (path, cb) => {
    return request
      .get({
        url: base_url + path,
        'servant-session-id': sessionId
      }, cb)
  },

  post: (path, payload, cb) => {
    return request
      .post({
        url: base_url + path,
        'servant-session-id': sessionId
      }, cb)
      .json(payload)
  }
})

const post = (path, payload, cb) => {
  return request
    .post(base_url + path, cb)
    .json(payload)
}

export default {
  base_url,
  authenticate,
  post
}


