export default {

  isAuthenticated: (state = false, action) => {
    switch(action.type) {
      case 'AUTH_SUCCESS':
        return true
      default:
        return false
    }
  },

  session: (state = null, action) => {
    switch(action.type) {
      case 'AUTH_SUCCESS':
        return action.session
      default:
        return state
    }
  }
}
