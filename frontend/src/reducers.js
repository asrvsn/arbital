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

  dataReloadListeners: (state = {}, action) => {
    const fireListeners = (dataSource) => {
      console.warn('fireListeners', dataSource)
      Object.values(state).forEach(
        listener => listener(dataSource)
      )
    }

    switch(action.type) {

      case 'REGISTER_RELOAD_LISTENER': {
        const { name, listener } = action
        console.warn('REGISTER_RELOAD_LISTENER', name)
        const state_ = Object.assign({}, state)
        state_[name] = listener
        return state_
      }

      case 'UNREGISTER_RELOAD_LISTENER': {
        const { name } = action
        console.warn('UNREGISTER_RELOAD_LISTENER', name)
        const state_ = Object.assign({}, state)
        delete state_[name]
        return state_
      }

      case 'FIRE_RELOAD': {
        console.warn('FIRE_RELOAD', action.dataSource)
        // XXX (anand) this does effectful things. Figure out a better way.
        fireListeners(action.datasource)
        return state
      }

      case 'INTERACT_DIALOG_STACK': {
        const { pushState } = action
        switch(pushState.action) {
          case 'PUSH': {
            return state
          }
          case 'POP': {
            switch(pushState.dialogType) {
              case 'CREATE_CLAIM': {
                // refresh all components listening to the claims datasource
                // when a new claim has been produced by the dialog
                if (!! pushState.props.createdClaim) {
                  fireListeners("CLAIMS")
                }
                return state
              }
              case 'CREATE_ARGUMENT': {
                // refresh all components listening to the arguments datasource
                // when a new argument has been produced by the dialog
                if (!! pushState.props.createdArgument) {
                  fireListeners("ARGUMENTS")
                }
                return state
              }
            }
          }
        }
        return state
      }

      default: {
        return state
      }
    }
  },

  lastDialogPushState: (state = null, action) => {
    switch(action.type) {
      case 'INTERACT_DIALOG_STACK': {
        console.warn('INTERACT_DIALOG_STACK', action.pushState)
        return action.pushState
      }
      default: {
        return state
      }
    }
  }
}
