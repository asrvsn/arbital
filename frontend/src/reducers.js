export default {

  isAuthenticated: (state = false, action) => {
    switch(action.type) {
      case 'AUTH_SUCCESS': {
        return true
      }
      default: {
        return state
      }
    }
  },

  session: (state = null, action) => {
    switch(action.type) {
      case 'AUTH_SUCCESS': {
        return action.session
      }
      default: {
        return state
      }
    }
  },

  claimsDirty: (state = false, action) => {
    switch(action.type) {
      case 'SET_CLAIMS_DIRTY':
        return action.state
      default:
        return state
    }
  }
}
