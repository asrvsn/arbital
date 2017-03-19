export const authSuccess = (session) => ({
  type: 'AUTH_SUCCESS',
  session
})

export const setClaimsDirty = (state) => ({
  type: 'SET_CLAIMS_DIRTY',
  state
})

export const setArgumentsDirty = (state) => ({
  type: 'SET_ARGUMENTS_DIRTY',
  state
})
